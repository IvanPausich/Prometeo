
Public Class ProcedureManager
    Private _TypeHandler As VBTypesHandler
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
        _CodeGen.AppendLine($"; Procedura {name}")
        _CodeGen.AppendLine($"{name}:")

        ' Salva tutti i registri
        _CodeGen.AppendLine("    push ebp")
        _CodeGen.AppendLine("    mov ebp, esp")         ' Setup frame stack
        _CodeGen.AppendLine("    push eax")
        _CodeGen.AppendLine("    push ebx")
        _CodeGen.AppendLine("    push ecx")
        _CodeGen.AppendLine("    push edx")
        _CodeGen.AppendLine("    push esi")
        _CodeGen.AppendLine("    push edi")

        ' Alloca spazio per parametri
        For i As Integer = 0 To parameters.Count - 1
            Dim param = parameters(i)
            Dim offset = (i + 2) * 4    ' +2 per saltare ebp salvato e indirizzo di ritorno
            ' Crea variabile temporanea per il parametro
            _MemoryManager.DeclareVariable($"_param_{name}_{i}", param.TypeName)
            ' Copia il valore dal parametro alla variabile temporanea
            _CodeGen.AppendLine($"    mov eax, [ebp+{offset}]")
            _CodeGen.AppendLine($"    mov edi, [_param_{name}_{i}]")
            _CodeGen.AppendLine($"    mov [edi], eax")
        Next
        _SymbolTable.EnterScope()
    End Sub

    Public Sub EndProcedure(parameters As List(Of Parameter))
        _LabelCounter += 1

        ' Dealloca parametri temporanei
        For i As Integer = 0 To parameters.Count - 1
            _MemoryManager.DeallocateVariable($"_param_{_CurrentProcedure}_{i}", parameters(i).TypeName)
        Next

        ' Label di uscita
        _CodeGen.AppendLine($"proc_exit_{_LabelCounter}:")
        _SymbolTable.ExitScope()
        ' Ripristina registri in ordine inverso
        _CodeGen.AppendLine("    pop edi")
        _CodeGen.AppendLine("    pop esi")
        _CodeGen.AppendLine("    pop edx")
        _CodeGen.AppendLine("    pop ecx")
        _CodeGen.AppendLine("    pop ebx")
        _CodeGen.AppendLine("    pop eax")
        _CodeGen.AppendLine("    mov esp, ebp")
        _CodeGen.AppendLine("    pop ebp")
        _CodeGen.AppendLine("    ret")
    End Sub

    Public Sub BeginFunction(name As String, returnType As String, parameters As List(Of Parameter))
        _CurrentProcedure = name
        _CodeGen.AppendLine($"; Funzione {name}")
        _CodeGen.AppendLine($"{name}:")
        ' Salva tutti i registri
        _CodeGen.AppendLine("    push ebp")
        _CodeGen.AppendLine("    mov ebp, esp")
        _CodeGen.AppendLine("    push eax")
        _CodeGen.AppendLine("    push ebx")
        _CodeGen.AppendLine("    push ecx")
        _CodeGen.AppendLine("    push edx")
        _CodeGen.AppendLine("    push esi")
        _CodeGen.AppendLine("    push edi")

        ' Crea variabile temporanea per il valore di ritorno
        _MemoryManager.DeclareVariable($"_return_{name}", returnType)

        ' Alloca spazio per parametri
        For i As Integer = 0 To parameters.Count - 1
            Dim param = parameters(i)
            Dim offset = (i + 2) * 4
            _MemoryManager.DeclareVariable($"_param_{name}_{i}", param.TypeName)
            _CodeGen.AppendLine($"    mov eax, [ebp+{offset}]")
            _CodeGen.AppendLine($"    mov edi, [_param_{name}_{i}]")
            _CodeGen.AppendLine($"    mov [edi], eax")
        Next
        _SymbolTable.EnterScope()
    End Sub

    Public Sub EndFunction(returnType As String, parameters As List(Of Parameter))
        _LabelCounter += 1

        ' Dealloca parametri temporanei
        For i As Integer = 0 To parameters.Count - 1
            _MemoryManager.DeallocateVariable($"_param_{_CurrentProcedure}_{i}", parameters(i).TypeName)
        Next

        ' Carica valore di ritorno in eax
        _CodeGen.AppendLine($"    mov edi, [_return_{_CurrentProcedure}]")
        Select Case _TypeHandler.GetTypeSize(returnType)
            Case 1
                _CodeGen.AppendLine("    mov al, [edi]")
            Case 2
                _CodeGen.AppendLine("    mov ax, [edi]")
            Case 4
                _CodeGen.AppendLine("    mov eax, [edi]")
        End Select

        ' Dealloca variabile di ritorno
        _MemoryManager.DeallocateVariable($"_return_{_CurrentProcedure}", returnType)
        _SymbolTable.ExitScope()
        ' Label di uscita
        _CodeGen.AppendLine($"func_exit_{_LabelCounter}:")
        ' Ripristina registri in ordine inverso
        _CodeGen.AppendLine("    pop edi")
        _CodeGen.AppendLine("    pop esi")
        _CodeGen.AppendLine("    pop edx")
        _CodeGen.AppendLine("    pop ecx")
        _CodeGen.AppendLine("    pop ebx")
        ' Non ripristiniamo eax perché contiene il valore di ritorno
        _CodeGen.AppendLine("    mov esp, ebp")
        _CodeGen.AppendLine("    pop ebp")
        _CodeGen.AppendLine("    ret")
    End Sub

    Public Sub CallProcedure(name As String, arguments As List(Of String))
        ' Push parametri in ordine inverso
        For i As Integer = arguments.Count - 1 To 0 Step -1
            _CodeGen.AppendLine($"    push {arguments(i)}")
        Next

        ' Chiama la procedura
        _CodeGen.AppendLine($"    call {name}")

        ' Rimuovi parametri dallo stack
        If arguments.Count > 0 Then
            _CodeGen.AppendLine($"    add esp, {arguments.Count * 4}")
        End If
    End Sub

    Public Sub CallFunction(name As String, resultVar As String, arguments As List(Of String))
        ' Push parametri in ordine inverso
        For i As Integer = arguments.Count - 1 To 0 Step -1
            _CodeGen.AppendLine($"    push {arguments(i)}")
        Next

        ' Chiama la funzione
        _CodeGen.AppendLine($"    call {name}")

        ' Rimuovi parametri dallo stack
        If arguments.Count > 0 Then
            _CodeGen.AppendLine($"    add esp, {arguments.Count * 4}")
        End If

        ' Salva risultato
        _CodeGen.AppendLine($"    mov edi, [{resultVar}]")
        _CodeGen.AppendLine("    mov [edi], eax")
    End Sub
End Class