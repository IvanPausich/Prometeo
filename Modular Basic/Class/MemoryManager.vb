
Public Class MemoryManager
    Private _LabelCounter As Integer = 0    ' Contatore per label uniche

    Public Sub New()
        GenerateMemoryCode()
    End Sub

    Private Sub GenerateMemoryCode()

        ' Funzione di allocazione memoria che usa HeaderAllocate
        _LibGen.Append("MemoryAlloc:")
        _LibGen.Append("    ; Input: ECX = dimensione richiesta")
        _LibGen.Append("    ; Output: EAX = indirizzo allocato o 0 se fallito")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Allineamento dimensione (facoltativo, per ottimizzazione)
        _LibGen.Append("    add ecx, 3")
        _LibGen.Append("    and ecx, 0xFFFFFFFC")   ' Allinea a 4 byte

        ' Chiamata a HeaderAllocate
        _LibGen.Append("    call HeaderAllocate")
        _LibGen.Append("    test eax, eax")         ' Verifica allocazione
        _LibGen.Append("    jz .alloc_failed")

        ' Setup memoria allocata
        _LibGen.Append("    push eax")              ' Salva indirizzo restituito
        _LibGen.Append("    mov edi, eax")          ' Prepara destinazione
        _LibGen.Append("    xor eax, eax")          ' Valore zero
        _LibGen.Append("    mov ecx, 4")            ' Inizializza almeno i primi 4 byte
        _LibGen.Append("    rep stosb")             ' Azzera memoria
        _LibGen.Append("    pop eax")               ' Ripristina indirizzo

        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")

        _LibGen.Append(".alloc_failed:")
        _LibGen.Append("    xor eax, eax")          ' Ritorna 0 per fallimento
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")

        ' Funzione di deallocazione memoria che usa HeaderDeallocate
        _LibGen.Append("MemoryDealloc:")
        _LibGen.Append("    ; Input: EAX = indirizzo da deallocare")
        _LibGen.Append("    ; Input: ECX = dimensione da deallocare")
        _LibGen.Append("    push ebx")
        _LibGen.Append("    push esi")
        _LibGen.Append("    push edi")

        ' Verifica indirizzo valido
        _LibGen.Append("    test eax, eax")
        _LibGen.Append("    jz .dealloc_done")

        ' Allineamento dimensione
        _LibGen.Append("    add ecx, 3")
        _LibGen.Append("    and ecx, 0xFFFFFFFC")   ' Allinea a 4 byte

        ' Chiamata a HeaderDeallocate
        _LibGen.Append("    call HeaderDeallocate")

        _LibGen.Append(".dealloc_done:")
        _LibGen.Append("    pop edi")
        _LibGen.Append("    pop esi")
        _LibGen.Append("    pop ebx")
        _LibGen.Append("    ret")
    End Sub

    Public Sub DeclareVariable(name As String, typeName As String, Optional arraySize As Integer = 0)

        ' Calcola dimensione
        Dim elementSize = _TypeHandler.GetTypeSize(typeName)
        Dim totalSize = If(arraySize > 0, elementSize * arraySize, elementSize)

        ' Alloca memoria
        _CodeGen.Append($"    ; Allocazione memoria per {name} ({typeName})")
        _CodeGen.Append("    push ecx")
        _CodeGen.Append($"    mov ecx, {totalSize}")
        _CodeGen.Append("    call MemoryAlloc")
        _CodeGen.Append($"    mov [{name}], eax")
        _CodeGen.Append("    pop ecx")

        ' Inizializza
        Dim defaultValue = _TypeHandler.GetDefaultValue(typeName)
        If arraySize > 0 Then
            _LabelCounter += 1    ' Incrementa contatore per label unica
            _CodeGen.Append("    push ecx")
            _CodeGen.Append("    push edi")
            _CodeGen.Append($"    mov edi, [{name}]")
            _CodeGen.Append($"    mov ecx, {arraySize}")
            _CodeGen.Append($"    mov eax, {defaultValue}")
            _CodeGen.Append($"init_{_LabelCounter}:")   ' Label numerata invece che basata sul nome
            Select Case elementSize
                Case 1
                    _CodeGen.Append("    mov byte [edi], al")
                    _CodeGen.Append("    add edi, 1")
                Case 2
                    _CodeGen.Append("    mov word [edi], ax")
                    _CodeGen.Append("    add edi, 2")
                Case 4
                    _CodeGen.Append("    mov dword [edi], eax")
                    _CodeGen.Append("    add edi, 4")
            End Select
            _CodeGen.Append($"    loop init_{_LabelCounter}")
            _CodeGen.Append("    pop edi")
            _CodeGen.Append("    pop ecx")
        Else
            _CodeGen.Append($"    mov edi, [{name}]")
            Select Case elementSize
                Case 1
                    _CodeGen.Append($"    mov byte [edi], {defaultValue}")
                Case 2
                    _CodeGen.Append($"    mov word [edi], {defaultValue}")
                Case 4
                    _CodeGen.Append($"    mov dword [edi], {defaultValue}")
            End Select
        End If
    End Sub

    Public Sub DeallocateVariable(name As String, typeName As String, Optional arraySize As Integer = 0)
        ' Calcola la dimensione da deallocare
        Dim elementSize = _TypeHandler.GetTypeSize(typeName)
        Dim totalSize = If(arraySize > 0, elementSize * arraySize, elementSize)
        _LabelCounter += 1    ' Incrementa contatore per label unica
        ' Genera il codice per la deallocazione
        _CodeGen.Append($"    ; Deallocazione {name}")
        _CodeGen.Append("    push eax")
        _CodeGen.Append("    push ecx")
        _CodeGen.Append($"    mov eax, [{name}]")
        _CodeGen.Append("    test eax, eax")
        _CodeGen.Append($"    jz .skip_dealloc_{_LabelCounter}")
        _CodeGen.Append($"    mov ecx, {totalSize}")
        _CodeGen.Append("    call MemoryDealloc")
        _CodeGen.Append($".skip_dealloc_{_LabelCounter}:")
        _CodeGen.Append("    pop ecx")
        _CodeGen.Append("    pop eax")
    End Sub

    ' Metodo helper per gestire la ridimensione degli array
    Public Sub ResizeArray(name As String, typeName As String, newSize As Integer, oldSize As Integer)
        Dim elementSize = _TypeHandler.GetTypeSize(typeName)
        Dim newTotalSize = elementSize * newSize
        _LabelCounter += 1    ' Incrementa contatore per label unica
        _CodeGen.Append($"    ; Ridimensionamento array {name}")
        _CodeGen.Append("    push eax")
        _CodeGen.Append("    push ebx")
        _CodeGen.Append("    push ecx")
        _CodeGen.Append("    push esi")
        _CodeGen.Append("    push edi")

        ' Alloca nuovo spazio
        _CodeGen.Append($"    mov ecx, {newTotalSize}")
        _CodeGen.Append("    call MemoryAlloc")
        _CodeGen.Append("    test eax, eax")
        _CodeGen.Append($"    jz .resize_failed_{_LabelCounter}")

        ' Copia dati vecchi
        _CodeGen.Append("    mov edi, eax")           ' Destinazione
        _CodeGen.Append($"    mov esi, [{name}]")     ' Sorgente
        _CodeGen.Append($"    mov ecx, {Math.Min(newSize, oldSize)}")  ' Minimo tra vecchia e nuova dimensione

        Select Case elementSize
            Case 1
                _CodeGen.Append("    rep movsb")
            Case 2
                _CodeGen.Append("    rep movsw")
            Case 4
                _CodeGen.Append("    rep movsd")
        End Select

        ' Se il nuovo array è più grande, inizializza la parte extra
        If newSize > oldSize Then
            Dim defaultValue = _TypeHandler.GetDefaultValue(typeName)
            _CodeGen.Append($"    mov ecx, {newSize - oldSize}")
            _CodeGen.Append($"    mov eax, {defaultValue}")
            _CodeGen.Append($".init_extra_{_LabelCounter}:")
            Select Case elementSize
                Case 1
                    _CodeGen.Append("    mov byte [edi], al")
                    _CodeGen.Append("    add edi, 1")
                Case 2
                    _CodeGen.Append("    mov word [edi], ax")
                    _CodeGen.Append("    add edi, 2")
                Case 4
                    _CodeGen.Append("    mov dword [edi], eax")
                    _CodeGen.Append("    add edi, 4")
            End Select
            _CodeGen.Append($"    loop .init_extra_{_LabelCounter}")
        End If

        ' Dealloca vecchio array
        _CodeGen.Append($"    mov eax, [{name}]")
        _CodeGen.Append($"    mov ecx, {elementSize * oldSize}")
        _CodeGen.Append("    call MemoryDealloc")

        ' Aggiorna puntatore
        _CodeGen.Append($"    mov [{name}], edi")

        _CodeGen.Append($".resize_failed_{_LabelCounter}:")
        _CodeGen.Append("    pop edi")
        _CodeGen.Append("    pop esi")
        _CodeGen.Append("    pop ecx")
        _CodeGen.Append("    pop ebx")
        _CodeGen.Append("    pop eax")
    End Sub
End Class