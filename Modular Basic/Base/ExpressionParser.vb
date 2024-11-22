Public Class ExpressionParser
    Private _TypeHandler As New VBTypesHandler()
    Private _MathOps As New MathOperationsGenerator()
    Private _StringHandler As New StringHandler()
    Private _ProcManager As New ProcedureManager()
    Private _TempVarCounter As Integer = 0

    Private Structure sOperator
        Public Token As String
        Public Precedence As Integer
        Public IsFunction As Boolean
        Public IsUserFunction As Boolean  ' Aggiunto per distinguere funzioni utente
    End Structure

    Private ReadOnly OperatorPrecedence As New Dictionary(Of String, Integer) From {
        {"(", 0},
        {"&", 1},
        {"+", 2},
        {"-", 2},
        {"*", 3},
        {"/", 3},
        {"^", 4},
        {"SQRT", 5},
        {"SIN", 5},
        {"COS", 5},
        {"TAN", 5},
        {"LEFT", 5},
        {"RIGHT", 5},
        {"MID", 5},
        {"LEN", 5},
        {"UCASE", 5},
        {"LCASE", 5},
        {"TRIM", 5}
    }

    Public Function ParseExpression(startIndex As Integer, ByRef resultType As String) As String
        Dim operators As New Stack(Of sOperator)()
        Dim output As New Stack(Of String)()
        Dim currentIndex As Integer = startIndex
        Dim functionArgs As New Stack(Of Integer)()  ' Per tenere traccia del numero di argomenti

        While currentIndex < tokens.Count
            Dim token = tokens(currentIndex)

            Select Case token.Symbol_ID
                Case VBParser.Tok_Types.LPARM
                    If operators.Count > 0 AndAlso operators.Peek().IsFunction Then
                        functionArgs.Push(1)  ' Inizia conteggio argomenti
                    End If
                    operators.Push(New sOperator With {
                        .Token = "(",
                        .Precedence = 0,
                        .IsFunction = False,
                        .IsUserFunction = False
                    })

                Case VBParser.Tok_Types.RPARM
                    ' Processa operatori fino alla parentesi aperta
                    While operators.Count > 0 AndAlso operators.Peek().Token <> "("
                        ProcessOperator(operators.Pop(), output, If(functionArgs.Count > 0, functionArgs.Peek(), 0))
                    End While
                    If operators.Count > 0 Then operators.Pop() ' Rimuove "("
                    ' Se era una funzione, processala
                    If operators.Count > 0 AndAlso operators.Peek().IsFunction Then
                        Dim op = operators.Pop()
                        Dim argCount = If(functionArgs.Count > 0, functionArgs.Pop(), 1)
                        ProcessOperator(op, output, argCount)
                    End If

                Case VBParser.Tok_Types.DIGIT
                    output.Push(CreateNumberConstant(token.Token))
                    resultType = "INTEGER"

                Case VBParser.Tok_Types.QSTRING
                    output.Push(CreateStringConstant(token.Token))
                    resultType = "STRING"

                Case VBParser.Tok_Types.LSTRING
                    Dim upperToken = token.Token.ToUpper()
                    If OperatorPrecedence.ContainsKey(upperToken) Then
                        ' È un operatore o una funzione built-in
                        Dim newOp As New sOperator With {
                            .Token = upperToken,
                            .Precedence = OperatorPrecedence(upperToken),
                            .IsFunction = IsFunction(upperToken),
                            .IsUserFunction = False
                        }
                        HandleOperator(newOp, operators, output)
                    ElseIf IsUserDefinedFunction(token.Token) Then
                        ' È una funzione definita dall'utente
                        Dim newOp As New sOperator With {
                            .Token = token.Token,
                            .Precedence = 5,  ' Alta priorità come altre funzioni
                            .IsFunction = True,
                            .IsUserFunction = True
                        }
                        HandleOperator(newOp, operators, output)
                    Else
                        ' È una variabile
                        output.Push(token.Token)
                        resultType = _SymbolTable.GetSymbolType(token.Token)
                    End If

                Case VBParser.Tok_Types.DEL
                    If token.Token = "," Then
                        ' Incrementa contatore argomenti
                        If functionArgs.Count > 0 Then
                            functionArgs.Push(functionArgs.Pop() + 1)
                        End If
                        ' Processa operatori fino alla parentesi
                        While operators.Count > 0 AndAlso operators.Peek().Token <> "("
                            ProcessOperator(operators.Pop(), output, 1)
                        End While
                    End If

                Case Else
                    ' Fine espressione
                    Exit While
            End Select

            currentIndex += 1
        End While

        ' Processa operatori rimanenti
        While operators.Count > 0
            ProcessOperator(operators.Pop(), output, 1)
        End While

        Return output.Pop()
    End Function

    Private Sub HandleOperator(newOp As sOperator, operators As Stack(Of sOperator), output As Stack(Of String))
        While operators.Count > 0 AndAlso
              operators.Peek().Precedence >= newOp.Precedence AndAlso
              operators.Peek().Token <> "("
            ProcessOperator(operators.Pop(), output, 1)
        End While
        operators.Push(newOp)
    End Sub

    Private Function IsUserDefinedFunction(name As String) As Boolean
        Return _SymbolTable.GetSymbol(name)?.Type = "FUNCTION"
    End Function

    Private Function CreateTempVar(type As String) As String
        _TempVarCounter += 1
        Dim tempName = $"_temp_{_TempVarCounter}"
        _MemoryManager.DeclareVariable(tempName, type)
        Return tempName
    End Function

    Private Function CreateNumberConstant(value As String) As String
        Dim tempVar = CreateTempVar("INTEGER")
        _CodeGen.AppendLine($"    mov edi, [{tempVar}]")
        _CodeGen.AppendLine($"    mov dword [edi], {value}")
        Return tempVar
    End Function

    Private Function CreateStringConstant(value As String) As String
        Dim tempVar = CreateTempVar("STRING")
        _CodeGen.AppendLine($"    mov edi, [{tempVar}]")
        _CodeGen.AppendLine($"    mov dword [edi], {value}")
        Return tempVar
    End Function

    Private Function IsFunction(token As String) As Boolean
        Return {"SQRT", "SIN", "COS", "TAN", "LEFT", "RIGHT", "MID",
                "LEN", "UCASE", "LCASE", "TRIM"}.Contains(token)
    End Function

    Private Sub ProcessOperator(op As sOperator, output As Stack(Of String), argCount As Integer)
        Dim resultVar = CreateTempVar(DetermineResultType(op, output, argCount))

        If op.IsFunction Then
            If op.IsUserFunction Then
                ProcessUserFunction(op.Token, resultVar, output, argCount)
            Else
                ProcessBuiltInFunction(op.Token, resultVar, output, argCount)
            End If
        Else
            Select Case op.Token
                Case "+", "-", "*", "/", "^"
                    ProcessMathOperation(op.Token, resultVar, output)
                Case "&"
                    ProcessStringConcat(resultVar, output)
            End Select
        End If

        output.Push(resultVar)
    End Sub

    Private Sub ProcessUserFunction(funcName As String, resultVar As String, operands As Stack(Of String), argCount As Integer)
        ' Prepara gli argomenti per la chiamata
        Dim args As New List(Of String)
        For i As Integer = 1 To argCount
            args.Insert(0, operands.Pop())  ' Inverti ordine per corretto pushing
        Next

        ' Chiama la funzione utente tramite ProcedureManager
        _ProcManager.CallFunction(funcName, resultVar, args)
    End Sub

    Private Sub ProcessBuiltInFunction(funcName As String, resultVar As String, operands As Stack(Of String), argCount As Integer)
        Select Case funcName
            Case "SQRT", "SIN", "COS", "TAN"
                Dim arg = operands.Pop()
                _MathOps.GenerateOperation(funcName, resultVar, arg, Nothing, "SINGLE")

            Case "LEFT", "RIGHT", "MID"
                Dim args = New List(Of String)
                For i As Integer = 1 To argCount
                    args.Insert(0, operands.Pop())
                Next
                Dim start = If(funcName = "MID" AndAlso args.Count > 2, args(0), Nothing)
                Dim length = If(funcName = "MID", args(1), args(0))
                Dim source = If(funcName = "MID", args(2), args(1))
                _StringHandler.GenerateStringOperation(funcName, resultVar, source, length, start)

            Case "LEN", "UCASE", "LCASE", "TRIM"
                Dim source = operands.Pop()
                _StringHandler.GenerateStringOperation(funcName, resultVar, source)
        End Select
    End Sub

    Private Function DetermineResultType(op As sOperator, output As Stack(Of String), argCount As Integer) As String
        If op.IsFunction Then
            If op.IsUserFunction Then
                Return _SymbolTable.GetSymbol(op.Token).Type
            End If

            Select Case op.Token
                Case "LEFT", "RIGHT", "MID", "UCASE", "LCASE", "TRIM"
                    Return "STRING"
                Case "LEN"
                    Return "INTEGER"
                Case Else
                    Return "SINGLE"  ' Per funzioni matematiche
            End Select
        Else
            Dim rightType = _SymbolTable.GetSymbolType(output.Pop())
            Dim leftType = If(output.Count > 0, _SymbolTable.GetSymbolType(output.Pop()), rightType)

            If op.Token = "&" Then Return "STRING"
            If leftType = "STRING" Or rightType = "STRING" Then
                ErrMessage("Cannot perform arithmetic on strings")
                Return "INTEGER"
            End If
            If leftType = "SINGLE" Or rightType = "SINGLE" Then Return "SINGLE"
            Return "INTEGER"
        End If
    End Function

    Private Sub ProcessMathOperation(op As String, result As String, operands As Stack(Of String))
        Dim right = operands.Pop()
        Dim left = operands.Pop()
        Dim resultType = _SymbolTable.GetSymbolType(result)
        _MathOps.GenerateOperation(op, result, left, right, resultType)
    End Sub

    Private Sub ProcessStringConcat(result As String, operands As Stack(Of String))
        Dim right = operands.Pop()
        Dim left = operands.Pop()
        _StringHandler.GenerateStringOperation("&", result, left, right)
    End Sub
End Class