Imports System.Text

Public Class MemoryManager
    Private _HeaderManager As MemoryHeaderManager
    Private _TypeHandler As VBTypesHandler
    Private _LabelCounter As Integer = 0    ' Contatore per label uniche

    Public Sub New()
        _HeaderManager = New MemoryHeaderManager()
        _TypeHandler = New VBTypesHandler()
        GenerateMemoryCode()
    End Sub

    Private Sub GenerateMemoryCode()

        ' Funzione di allocazione memoria che usa HeaderAllocate
        _LibGen.AppendLine("MemoryAlloc:")
        _LibGen.AppendLine("    ; Input: ECX = dimensione richiesta")
        _LibGen.AppendLine("    ; Output: EAX = indirizzo allocato o 0 se fallito")
        _LibGen.AppendLine("    push ebx")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Allineamento dimensione (facoltativo, per ottimizzazione)
        _LibGen.AppendLine("    add ecx, 3")
        _LibGen.AppendLine("    and ecx, 0xFFFFFFFC")   ' Allinea a 4 byte

        ' Chiamata a HeaderAllocate
        _LibGen.AppendLine("    call HeaderAllocate")
        _LibGen.AppendLine("    test eax, eax")         ' Verifica allocazione
        _LibGen.AppendLine("    jz .alloc_failed")

        ' Setup memoria allocata
        _LibGen.AppendLine("    push eax")              ' Salva indirizzo restituito
        _LibGen.AppendLine("    mov edi, eax")          ' Prepara destinazione
        _LibGen.AppendLine("    xor eax, eax")          ' Valore zero
        _LibGen.AppendLine("    mov ecx, 4")            ' Inizializza almeno i primi 4 byte
        _LibGen.AppendLine("    rep stosb")             ' Azzera memoria
        _LibGen.AppendLine("    pop eax")               ' Ripristina indirizzo

        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    ret")

        _LibGen.AppendLine(".alloc_failed:")
        _LibGen.AppendLine("    xor eax, eax")          ' Ritorna 0 per fallimento
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    ret")

        ' Funzione di deallocazione memoria che usa HeaderDeallocate
        _LibGen.AppendLine("MemoryDealloc:")
        _LibGen.AppendLine("    ; Input: EAX = indirizzo da deallocare")
        _LibGen.AppendLine("    ; Input: ECX = dimensione da deallocare")
        _LibGen.AppendLine("    push ebx")
        _LibGen.AppendLine("    push esi")
        _LibGen.AppendLine("    push edi")

        ' Verifica indirizzo valido
        _LibGen.AppendLine("    test eax, eax")
        _LibGen.AppendLine("    jz .dealloc_done")

        ' Allineamento dimensione
        _LibGen.AppendLine("    add ecx, 3")
        _LibGen.AppendLine("    and ecx, 0xFFFFFFFC")   ' Allinea a 4 byte

        ' Chiamata a HeaderDeallocate
        _LibGen.AppendLine("    call HeaderDeallocate")

        _LibGen.AppendLine(".dealloc_done:")
        _LibGen.AppendLine("    pop edi")
        _LibGen.AppendLine("    pop esi")
        _LibGen.AppendLine("    pop ebx")
        _LibGen.AppendLine("    ret")
    End Sub

    Public Sub DeclareVariable(name As String, typeName As String, Optional arraySize As Integer = 0)
        ' Dichiara variabile statica nella sezione .data
        _CodeGen.AppendLine($"{name} dd 0   ; Puntatore alla memoria allocata")

        ' Calcola dimensione
        Dim elementSize = _TypeHandler.GetTypeSize(typeName)
        Dim totalSize = If(arraySize > 0, elementSize * arraySize, elementSize)

        ' Alloca memoria
        _CodeGen.AppendLine($"    ; Allocazione memoria per {name} ({typeName})")
        _CodeGen.AppendLine("    push ecx")
        _CodeGen.AppendLine($"    mov ecx, {totalSize}")
        _CodeGen.AppendLine("    call MemoryAlloc")
        _CodeGen.AppendLine("    test eax, eax")
        _CodeGen.AppendLine("    jz allocation_failed")
        _CodeGen.AppendLine($"    mov [{name}], eax")
        _CodeGen.AppendLine("    pop ecx")

        ' Inizializza
        Dim defaultValue = _TypeHandler.GetDefaultValue(typeName)
        If arraySize > 0 Then
            _LabelCounter += 1    ' Incrementa contatore per label unica
            _CodeGen.AppendLine("    push ecx")
            _CodeGen.AppendLine("    push edi")
            _CodeGen.AppendLine($"    mov edi, [{name}]")
            _CodeGen.AppendLine($"    mov ecx, {arraySize}")
            _CodeGen.AppendLine($"    mov eax, {defaultValue}")
            _CodeGen.AppendLine($"init_{_LabelCounter}:")   ' Label numerata invece che basata sul nome
            Select Case elementSize
                Case 1
                    _CodeGen.AppendLine("    mov byte [edi], al")
                    _CodeGen.AppendLine("    add edi, 1")
                Case 2
                    _CodeGen.AppendLine("    mov word [edi], ax")
                    _CodeGen.AppendLine("    add edi, 2")
                Case 4
                    _CodeGen.AppendLine("    mov dword [edi], eax")
                    _CodeGen.AppendLine("    add edi, 4")
            End Select
            _CodeGen.AppendLine($"    loop init_{_LabelCounter}")
            _CodeGen.AppendLine("    pop edi")
            _CodeGen.AppendLine("    pop ecx")
        Else
            _CodeGen.AppendLine($"    mov edi, [{name}]")
            Select Case elementSize
                Case 1
                    _CodeGen.AppendLine($"    mov byte [edi], {defaultValue}")
                Case 2
                    _CodeGen.AppendLine($"    mov word [edi], {defaultValue}")
                Case 4
                    _CodeGen.AppendLine($"    mov dword [edi], {defaultValue}")
            End Select
        End If
    End Sub

    Public Sub DeallocateVariable(name As String, typeName As String, Optional arraySize As Integer = 0)
        ' Calcola la dimensione da deallocare
        Dim elementSize = _TypeHandler.GetTypeSize(typeName)
        Dim totalSize = If(arraySize > 0, elementSize * arraySize, elementSize)
        _LabelCounter += 1    ' Incrementa contatore per label unica
        ' Genera il codice per la deallocazione
        _CodeGen.AppendLine($"    ; Deallocazione {name}")
        _CodeGen.AppendLine("    push eax")
        _CodeGen.AppendLine("    push ecx")
        _CodeGen.AppendLine($"    mov eax, [{name}]")
        _CodeGen.AppendLine("    test eax, eax")
        _CodeGen.AppendLine($"    jz .skip_dealloc_{_LabelCounter}")
        _CodeGen.AppendLine($"    mov ecx, {totalSize}")
        _CodeGen.AppendLine("    call MemoryDealloc")
        _CodeGen.AppendLine($".skip_dealloc_{_LabelCounter}:")
        _CodeGen.AppendLine("    pop ecx")
        _CodeGen.AppendLine("    pop eax")
    End Sub

    ' Metodo helper per gestire la ridimensione degli array
    Public Sub ResizeArray(name As String, typeName As String, newSize As Integer, oldSize As Integer)
        Dim elementSize = _TypeHandler.GetTypeSize(typeName)
        Dim newTotalSize = elementSize * newSize
        _LabelCounter += 1    ' Incrementa contatore per label unica
        _CodeGen.AppendLine($"    ; Ridimensionamento array {name}")
        _CodeGen.AppendLine("    push eax")
        _CodeGen.AppendLine("    push ebx")
        _CodeGen.AppendLine("    push ecx")
        _CodeGen.AppendLine("    push esi")
        _CodeGen.AppendLine("    push edi")

        ' Alloca nuovo spazio
        _CodeGen.AppendLine($"    mov ecx, {newTotalSize}")
        _CodeGen.AppendLine("    call MemoryAlloc")
        _CodeGen.AppendLine("    test eax, eax")
        _CodeGen.AppendLine($"    jz .resize_failed_{_LabelCounter}")

        ' Copia dati vecchi
        _CodeGen.AppendLine("    mov edi, eax")           ' Destinazione
        _CodeGen.AppendLine($"    mov esi, [{name}]")     ' Sorgente
        _CodeGen.AppendLine($"    mov ecx, {Math.Min(newSize, oldSize)}")  ' Minimo tra vecchia e nuova dimensione

        Select Case elementSize
            Case 1
                _CodeGen.AppendLine("    rep movsb")
            Case 2
                _CodeGen.AppendLine("    rep movsw")
            Case 4
                _CodeGen.AppendLine("    rep movsd")
        End Select

        ' Se il nuovo array è più grande, inizializza la parte extra
        If newSize > oldSize Then
            Dim defaultValue = _TypeHandler.GetDefaultValue(typeName)
            _CodeGen.AppendLine($"    mov ecx, {newSize - oldSize}")
            _CodeGen.AppendLine($"    mov eax, {defaultValue}")
            _CodeGen.AppendLine($".init_extra_{_LabelCounter}:")
            Select Case elementSize
                Case 1
                    _CodeGen.AppendLine("    mov byte [edi], al")
                    _CodeGen.AppendLine("    add edi, 1")
                Case 2
                    _CodeGen.AppendLine("    mov word [edi], ax")
                    _CodeGen.AppendLine("    add edi, 2")
                Case 4
                    _CodeGen.AppendLine("    mov dword [edi], eax")
                    _CodeGen.AppendLine("    add edi, 4")
            End Select
            _CodeGen.AppendLine($"    loop .init_extra_{_LabelCounter}")
        End If

        ' Dealloca vecchio array
        _CodeGen.AppendLine($"    mov eax, [{name}]")
        _CodeGen.AppendLine($"    mov ecx, {elementSize * oldSize}")
        _CodeGen.AppendLine("    call MemoryDealloc")

        ' Aggiorna puntatore
        _CodeGen.AppendLine($"    mov [{name}], edi")

        _CodeGen.AppendLine($".resize_failed_{_LabelCounter}:")
        _CodeGen.AppendLine("    pop edi")
        _CodeGen.AppendLine("    pop esi")
        _CodeGen.AppendLine("    pop ecx")
        _CodeGen.AppendLine("    pop ebx")
        _CodeGen.AppendLine("    pop eax")
    End Sub
End Class