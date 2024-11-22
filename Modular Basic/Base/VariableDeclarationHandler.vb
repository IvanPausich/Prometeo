Public Class VariableDeclarationHandler
    Private _currentAccessLevel As String = "PRIVATE"  ' Default access level

    Public Function HandleDeclaration() As Boolean
        ' Controlla se siamo in una dichiarazione
        If Not tokens(currentIndex).Token.ToUpper() = "DIM" AndAlso
           Not tokens(currentIndex).Token.ToUpper() = "PUBLIC" AndAlso
           Not tokens(currentIndex).Token.ToUpper() = "PRIVATE" Then
            Return False
        End If

        ' Salva il livello di accesso
        _currentAccessLevel = tokens(currentIndex).Token.ToUpper()
        currentIndex += 1

        ' Analizza ogni variabile nella dichiarazione (supporta multiple con virgola)
        Do
            Dim varName = tokens(currentIndex).Token
            currentIndex += 1

            ' Controlla se è un array
            Dim arraySize As Integer = 0
            If currentIndex < tokens.Count AndAlso tokens(currentIndex).Token = "(" Then
                currentIndex += 1
                arraySize = Integer.Parse(tokens(currentIndex).Token)
                currentIndex += 2  ' Salta il numero e la parentesi chiusa
            End If

            ' Verifica "As"
            If Not tokens(currentIndex).Token.ToUpper() = "AS" Then
                ErrMessage("Expected 'As' in variable declaration")
                Return False
            End If
            currentIndex += 1

            ' Ottiene il tipo
            Dim varType = tokens(currentIndex).Token.ToUpper()
            currentIndex += 1

            ' Registra la variabile
            _SymbolTable.DeclareVariable(varName, varType, arraySize)

            ' Controlla se ci sono altre variabili (separate da virgola)
            If currentIndex >= tokens.Count OrElse tokens(currentIndex).Token <> "," Then
                Exit Do
            End If
            currentIndex += 1  ' Salta la virgola
        Loop

        Return True
    End Function
End Class