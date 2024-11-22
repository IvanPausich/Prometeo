Public Class CodeGenerator
    Private _expressionParser As New ExpressionParser()
    Private _procedureManager As New ProcedureManager()
    Private _varDeclarationHandler As New VariableDeclarationHandler()
    Private _mathOps As New MathOperationsGenerator()
    Private _stringHandler As New StringHandler()
    Private _currentScope As Integer = 0
    Private _currentProcedure As String = ""
    Private _inFunction As Boolean = False
    Private _returnType As String = ""

    Public Sub GenerateCode()
        ' Inizializza le sezioni del codice
        Dim currentIndex As Integer = 0
        While currentIndex < tokens.Count
            Dim token = tokens(currentIndex)
            Console.WriteLine(token.Token.ToUpper() & " " & currentIndex & " " & tokens.Count)

            Select Case token.Token.ToUpper()
                Case "FUNCTION"
                    currentIndex = HandleFunction(tokens, currentIndex)

                Case "SUB"
                    currentIndex = HandleSubroutine(tokens, currentIndex)

                Case "END"
                    currentIndex = HandleEndBlock(tokens, currentIndex)
                    Stop
                Case "DIM", "PUBLIC", "PRIVATE"
                    currentIndex = HandleVariableDeclaration(tokens, currentIndex)

                Case "IF"
                    currentIndex = HandleIfStatement(tokens, currentIndex)

                Case "FOR"
                    currentIndex = HandleForLoop(tokens, currentIndex)

                Case "WHILE"
                    currentIndex = HandleWhileLoop(tokens, currentIndex)

                Case "RETURN"
                    currentIndex = HandleReturn(tokens, currentIndex)

                Case Else
                    ' Controlla se è un'assegnazione
                    If currentIndex + 1 < tokens.Count AndAlso tokens(currentIndex + 1).Token = "=" Then
                        currentIndex = HandleAssignment(tokens, currentIndex)
                    ElseIf _SymbolTable.SymbolExists(token.Token) Then
                        ' Potrebbe essere una chiamata a procedura
                        currentIndex = HandleProcedureCall(tokens, currentIndex)
                    Else
                        currentIndex += 1
                    End If
            End Select
        End While
    End Sub


    Private Function HandleFunction(tokens As List(Of VBParser.Parsed_Code), currentIndex As Integer) As Integer
        _inFunction = True
        currentIndex += 1 ' Salta "FUNCTION"

        ' Ottieni nome funzione
        Dim funcName = tokens(currentIndex).Token
        _currentProcedure = funcName
        currentIndex += 1

        ' Raccogli parametri
        Dim parameters As New List(Of ProcedureManager.Parameter)
        If tokens(currentIndex).Token = "(" Then
            currentIndex = CollectParameters(tokens, currentIndex, parameters)
        End If

        ' Ottieni tipo di ritorno
        currentIndex += 1 ' Salta "As"
        _returnType = tokens(currentIndex).Token
        currentIndex += 1

        ' Inizia la funzione
        _procedureManager.BeginFunction(funcName, _returnType, parameters)
        _currentScope += 1

        Return currentIndex
    End Function

    Private Function HandleSubroutine(tokens As List(Of VBParser.Parsed_Code), currentIndex As Integer) As Integer
        _inFunction = False
        currentIndex += 1 ' Salta "SUB"

        ' Ottieni nome subroutine
        Dim subName = tokens(currentIndex).Token
        _currentProcedure = subName
        currentIndex += 1

        ' Raccogli parametri
        Dim parameters As New List(Of ProcedureManager.Parameter)
        If tokens(currentIndex).Token = "(" Then
            currentIndex = CollectParameters(tokens, currentIndex, parameters)
        End If

        ' Inizia la subroutine
        _procedureManager.BeginProcedure(subName, parameters)
        _currentScope += 1

        Return currentIndex
    End Function

    Private Function CollectParameters(tokens As List(Of VBParser.Parsed_Code),
                                     startIndex As Integer,
                                     parameters As List(Of ProcedureManager.Parameter)) As Integer
        Dim currentIndex = startIndex + 1 ' Salta "("

        While tokens(currentIndex).Token <> ")"
            If tokens(currentIndex).Token <> "," Then
                Dim param As New ProcedureManager.Parameter
                param.Name = tokens(currentIndex).Token
                currentIndex += 2 ' Salta "As"
                param.TypeName = tokens(currentIndex).Token
                parameters.Add(param)
            End If
            currentIndex += 1
        End While

        Return currentIndex + 1 ' Salta ")"
    End Function

    Private Function HandleEndBlock(tokens As List(Of VBParser.Parsed_Code), currentIndex As Integer) As Integer
        currentIndex += 1 ' Salta "End"

        Select Case tokens(currentIndex).Token.ToUpper()
            Case "FUNCTION"
                Dim parameters As New List(Of ProcedureManager.Parameter)
                _procedureManager.EndFunction(_returnType, parameters)
                _inFunction = False
                _currentProcedure = ""
                _returnType = ""

            Case "SUB"
                Dim parameters As New List(Of ProcedureManager.Parameter)
                _procedureManager.EndProcedure(parameters)
                _currentProcedure = ""

            Case "IF"
                _CodeGen.AppendLine($"endif_{_currentScope}:")

            Case "FOR"
                _CodeGen.AppendLine($"    jmp forloop_{_currentScope}")
                _CodeGen.AppendLine($"fornext_{_currentScope}:")

            Case "WHILE"
                _CodeGen.AppendLine($"    jmp whilecheck_{_currentScope}")
                _CodeGen.AppendLine($"whileend_{_currentScope}:")
        End Select

        _currentScope -= 1
        Return currentIndex + 1
    End Function

    Private Function HandleVariableDeclaration(tokens As List(Of VBParser.Parsed_Code),
                                             currentIndex As Integer) As Integer
        Return _varDeclarationHandler.HandleDeclaration()
    End Function

    Private Function HandleAssignment(tokens As List(Of VBParser.Parsed_Code),
                                    currentIndex As Integer) As Integer
        Dim varName = tokens(currentIndex).Token
        currentIndex += 2 ' Salta "="
        Stop
        ' Verifica se è un'espressione
        Dim resultType As String = ""
        Dim result = _expressionParser.ParseExpression(currentIndex, resultType)

        ' Genera il codice di assegnazione
        _CodeGen.AppendLine($"    ; Assegnazione a {varName}")
        _CodeGen.AppendLine($"    mov edi, [{varName}]")
        _CodeGen.AppendLine($"    mov eax, [{result}]")
        _CodeGen.AppendLine("    mov [edi], eax")

        ' Salta fino alla fine dell'espressione
        While currentIndex < tokens.Count AndAlso tokens(currentIndex).Token <> vbCrLf
            currentIndex += 1
        End While

        Return currentIndex
    End Function

    Private Function HandleProcedureCall(tokens As List(Of VBParser.Parsed_Code),
                                       currentIndex As Integer) As Integer
        Dim procName = tokens(currentIndex).Token
        currentIndex += 1

        ' Raccogli argomenti
        Dim arguments As New List(Of String)
        If currentIndex < tokens.Count AndAlso tokens(currentIndex).Token = "(" Then
            currentIndex += 1 ' Salta "("
            While tokens(currentIndex).Token <> ")"
                If tokens(currentIndex).Token <> "," Then
                    Dim argType As String = ""
                    Dim arg = _expressionParser.ParseExpression(currentIndex, argType)
                    arguments.Add(arg)
                End If
                currentIndex += 1
            End While
            currentIndex += 1 ' Salta ")"
        End If

        ' Genera chiamata
        _procedureManager.CallProcedure(procName, arguments)

        Return currentIndex
    End Function

    Private Function HandleReturn(tokens As List(Of VBParser.Parsed_Code),
                                currentIndex As Integer) As Integer
        If _inFunction Then
            currentIndex += 1 ' Salta "Return"
            Dim resultType As String = ""
            Dim result = _expressionParser.ParseExpression(currentIndex, resultType)

            _CodeGen.AppendLine($"    ; Return value")
            _CodeGen.AppendLine($"    mov edi, [_return_{_currentProcedure}]")
            _CodeGen.AppendLine($"    mov eax, [{result}]")
            _CodeGen.AppendLine("    mov [edi], eax")
            _CodeGen.AppendLine($"    jmp func_exit_{_currentScope}")
        End If

        ' Salta fino alla fine dell'espressione
        While currentIndex < tokens.Count AndAlso tokens(currentIndex).Token <> vbCrLf
            currentIndex += 1
        End While

        Return currentIndex
    End Function

    Private Function HandleIfStatement(tokens As List(Of VBParser.Parsed_Code),
                                     currentIndex As Integer) As Integer
        _currentScope += 1
        currentIndex += 1 ' Salta "IF"

        ' Valuta condizione
        Dim conditionType As String = ""
        Dim condition = _expressionParser.ParseExpression(currentIndex, conditionType)

        _CodeGen.AppendLine($"    ; If statement")
        _CodeGen.AppendLine($"    mov eax, [{condition}]")
        _CodeGen.AppendLine("    test eax, eax")
        _CodeGen.AppendLine($"    jz endif_{_currentScope}")

        ' Salta fino a Then
        While currentIndex < tokens.Count AndAlso tokens(currentIndex).Token.ToUpper() <> "THEN"
            currentIndex += 1
        End While

        Return currentIndex + 1
    End Function

    Private Function HandleForLoop(tokens As List(Of VBParser.Parsed_Code),
                                 currentIndex As Integer) As Integer
        _currentScope += 1
        currentIndex += 1 ' Salta "FOR"

        ' Ottieni variabile contatore
        Dim counterVar = tokens(currentIndex).Token
        currentIndex += 2 ' Salta "="

        ' Ottieni valore iniziale
        Dim initType As String = ""
        Dim initValue = _expressionParser.ParseExpression(currentIndex, initType)

        ' Cerca "To"
        While currentIndex < tokens.Count AndAlso tokens(currentIndex).Token.ToUpper() <> "TO"
            currentIndex += 1
        End While
        currentIndex += 1 ' Salta "TO"

        ' Ottieni valore finale
        Dim endType As String = ""
        Dim endValue = _expressionParser.ParseExpression(currentIndex, endType)

        ' Genera codice per il loop
        _CodeGen.AppendLine($"    ; For loop initialization")
        _CodeGen.AppendLine($"    mov eax, [{initValue}]")
        _CodeGen.AppendLine($"    mov edi, [{counterVar}]")
        _CodeGen.AppendLine("    mov [edi], eax")

        _CodeGen.AppendLine($"forloop_{_currentScope}:")
        _CodeGen.AppendLine($"    mov edi, [{counterVar}]")
        _CodeGen.AppendLine("    mov eax, [edi]")
        _CodeGen.AppendLine($"    cmp eax, [{endValue}]")
        _CodeGen.AppendLine($"    jg fornext_{_currentScope}")

        Return currentIndex + 1
    End Function

    Private Function HandleWhileLoop(tokens As List(Of VBParser.Parsed_Code),
                                   currentIndex As Integer) As Integer
        _currentScope += 1
        currentIndex += 1 ' Salta "WHILE"

        _CodeGen.AppendLine($"whilecheck_{_currentScope}:")

        ' Valuta condizione
        Dim conditionType As String = ""
        Dim condition = _expressionParser.ParseExpression(currentIndex, conditionType)

        _CodeGen.AppendLine($"    ; While loop condition")
        _CodeGen.AppendLine($"    mov eax, [{condition}]")
        _CodeGen.AppendLine("    test eax, eax")
        _CodeGen.AppendLine($"    jz whileend_{_currentScope}")

        Return currentIndex + 1
    End Function
End Class