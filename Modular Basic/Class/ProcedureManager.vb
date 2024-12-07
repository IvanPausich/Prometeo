
Public Class ProcedureManager
    Private _LabelCounter As Integer = 0
    Private _CurrentProcedure As String = ""

    Public Structure Parameter
        Public Name As String
        Public TypeName As String
    End Structure

    Public Sub New()
        _TypeHandler = New VBTypesHandler()
    End Sub

    Public Sub BeginProcedure(name As String, parameters As List(Of Parameter))
        _CurrentProcedure = name
        _CodeGen.Append($"; Procedura {name}")
        _CodeGen.Append($"{name}:")

        ' Salva tutti i registri
        _CodeGen.Append("    push ebp")
        _CodeGen.Append("    mov ebp, esp")         ' Setup frame stack
        _CodeGen.Append("    push eax")
        _CodeGen.Append("    push ebx")
        _CodeGen.Append("    push ecx")
        _CodeGen.Append("    push edx")
        _CodeGen.Append("    push esi")
        _CodeGen.Append("    push edi")

        ' Alloca spazio per parametri
        For i As Integer = 0 To parameters.Count - 1
            Dim param = parameters(i)
            Dim offset = (i + 2) * 4    ' +2 per saltare ebp salvato e indirizzo di ritorno
            ' Crea variabile temporanea per il parametro
            _SymbolTable.DeclareVariable($"_param_{name}_{i}", param.TypeName)
            ' Copia il valore dal parametro alla variabile temporanea
            _CodeGen.Append($"    mov eax, [ebp+{offset}]")
            _CodeGen.Append($"    mov edi, [_param_{name}_{i}]")
            _CodeGen.Append($"    mov [edi], eax")
        Next
        _SymbolTable.EnterScope()
    End Sub

    Public Sub EndProcedure(parameters As List(Of Parameter))
        _LabelCounter += 1

        ' Dealloca parametri temporanei
        For i As Integer = 0 To parameters.Count - 1
            _SymbolTable.RemoveSymbol($"_param_{_CurrentProcedure}_{i}")
        Next

        Dim listOfVar As New List(Of String)
        listOfVar = _SymbolTable.VarScope
        ' Label di uscita
        _CodeGen.Append($"proc_exit_{_LabelCounter}:")
        _SymbolTable.ExitScope()
        ' Ripristina registri in ordine inverso
        _CodeGen.Append("    pop edi")
        _CodeGen.Append("    pop esi")
        _CodeGen.Append("    pop edx")
        _CodeGen.Append("    pop ecx")
        _CodeGen.Append("    pop ebx")
        _CodeGen.Append("    pop eax")
        _CodeGen.Append("    mov esp, ebp")
        _CodeGen.Append("    pop ebp")
        _CodeGen.Append("    ret")
        For i As Integer = 0 To listOfVar.Count - 1
            _CodeGen.Append($"{listOfVar(i)} DATA 0   ; Puntatore alla memoria allocata")
        Next

    End Sub

    Public Sub BeginFunction(name As String, returnType As String, parameters As List(Of Parameter))
        _CurrentProcedure = name
        _CodeGen.Append($"; Funzione {name}")
        _CodeGen.Append($"{name}:")
        ' Salva tutti i registri
        _CodeGen.Append("    push ebp")
        _CodeGen.Append("    mov ebp, esp")
        _CodeGen.Append("    push eax")
        _CodeGen.Append("    push ebx")
        _CodeGen.Append("    push ecx")
        _CodeGen.Append("    push edx")
        _CodeGen.Append("    push esi")
        _CodeGen.Append("    push edi")

        ' Crea variabile temporanea per il valore di ritorno
        _SymbolTable.DeclareVariable($"_return_{name}", returnType)

        ' Alloca spazio per parametri
        For i As Integer = 0 To parameters.Count - 1
            Dim param = parameters(i)
            Dim offset = (i + 2) * 4
            _SymbolTable.DeclareVariable($"_param_{name}_{i}", param.TypeName)
            _CodeGen.Append($"    mov eax, [ebp+{offset}]")
            _CodeGen.Append($"    mov edi, [_param_{name}_{i}]")
            _CodeGen.Append($"    mov [edi], eax")
        Next
        _SymbolTable.EnterScope()
    End Sub

    Public Sub EndFunction(returnType As String, parameters As List(Of Parameter))
        _LabelCounter += 1

        ' Dealloca parametri temporanei
        For i As Integer = 0 To parameters.Count - 1
            _SymbolTable.RemoveSymbol($"_param_{_CurrentProcedure}_{i}")
        Next

        ' Carica valore di ritorno in eax
        _CodeGen.Append($"    mov edi, [_return_{_CurrentProcedure}]")
        Select Case _TypeHandler.GetTypeSize(returnType)
            Case 1
                _CodeGen.Append("    mov al, [edi]")
            Case 2
                _CodeGen.Append("    mov ax, [edi]")
            Case 4
                _CodeGen.Append("    mov eax, [edi]")
        End Select
        Dim listOfVar As New List(Of String)
        listOfVar = _SymbolTable.VarScope
        ' Dealloca variabile di ritorno
        _SymbolTable.RemoveSymbol($"_return_{_CurrentProcedure}")
        _SymbolTable.ExitScope()
        ' Label di uscita
        _CodeGen.Append($"func_exit_{_LabelCounter}:")
        ' Ripristina registri in ordine inverso
        _CodeGen.Append("    pop edi")
        _CodeGen.Append("    pop esi")
        _CodeGen.Append("    pop edx")
        _CodeGen.Append("    pop ecx")
        _CodeGen.Append("    pop ebx")
        ' Non ripristiniamo eax perché contiene il valore di ritorno
        _CodeGen.Append("    mov esp, ebp")
        _CodeGen.Append("    pop ebp")
        _CodeGen.Append("    ret")
        ' Dichiara variabile statica nella sezione .data
        For i As Integer = 0 To listOfVar.Count - 1
            _CodeGen.Append($"{listOfVar(i)} DATA 0   ; Puntatore alla memoria allocata")
        Next

    End Sub

    Public Sub CallProcedure(name As String, arguments As List(Of String))
        ' Push parametri in ordine inverso
        For i As Integer = arguments.Count - 1 To 0 Step -1
            _CodeGen.Append($"    push {arguments(i)}")
        Next

        ' Chiama la procedura
        _CodeGen.Append($"    call {name}")

        ' Rimuovi parametri dallo stack
        If arguments.Count > 0 Then
            _CodeGen.Append($"    add esp, {arguments.Count * 4}")
        End If
    End Sub

    Public Sub CallFunction(name As String, resultVar As String, arguments As List(Of String))
        ' Push parametri in ordine inverso
        For i As Integer = arguments.Count - 1 To 0 Step -1
            _CodeGen.Append($"    push {arguments(i)}")
        Next

        ' Chiama la funzione
        _CodeGen.Append($"    call {name}")

        ' Rimuovi parametri dallo stack
        If arguments.Count > 0 Then
            _CodeGen.Append($"    add esp, {arguments.Count * 4}")
        End If

        ' Salva risultato
        _CodeGen.Append($"    mov edi, [{resultVar}]")
        _CodeGen.Append("    mov [edi], eax")
    End Sub
End Class