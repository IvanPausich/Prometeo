Public Class VBParser
    'Token Types
    Enum Tok_Types
        NONE = 0
        LSTRING = 1
        DIGIT = 2
        QSTRING = 3
        LPARM = 4
        RPARM = 5
        LVARIABLE = 6
        COMMENT = 7
        DEL = 8
        EOL = 9
        EOP = 10
    End Enum

    'Current processing char
    Public CharPos As Integer
    'Source to scan
    Private Source As String

    'Hold the returned Token
    Private Token As String
    'Hold the Token Type
    Private TokType As Tok_Types
    Public Class Parsed_Code
        Public Token As String
        Public Symbol As String
        Public Symbol_ID As Tok_Types
    End Class


    'Logging
    Private Log As String

        Public Sub New(code As String)
            Source = code
            CharPos = 1
        End Sub

        Function GetStrToken(iTokT As Tok_Types) As String
            Select Case iTokT
                Case Tok_Types.NONE
                    Return "Nothing"
                Case Tok_Types.LSTRING
                    Return "String"
                Case Tok_Types.DIGIT
                    Return "Number"
                Case Tok_Types.QSTRING
                    Return "Quote_String"
                Case Tok_Types.LPARM
                    Return "LPARM"
                Case Tok_Types.RPARM
                    Return "RPARM"
                Case Tok_Types.LVARIABLE
                    Return "Variable"
                Case Tok_Types.COMMENT
                    Return "Comment"
                Case Tok_Types.DEL
                    Return "Delimiter"
                Case Tok_Types.EOL
                    Return "EOL"
                Case Tok_Types.EOP
                    Return "EOP"
                Case Else
                    Return "UNKNOWN"
            End Select
        End Function

        Public Function IsAlpha(c As Char) As Boolean
            Return Char.IsLetter(c)
        End Function

        Public Function IsWhite(c As Char) As Boolean
            Return Char.IsWhiteSpace(c)
        End Function

        Public Function IsDigit(c As Char) As Boolean
            Return Char.IsDigit(c)
        End Function

        Function IsDelim(c As Char) As Boolean
            Return " ,;<>+-/*%^=[]()&".Contains(c) OrElse c = ControlChars.Cr
        End Function

        Sub INC(Optional nMove As Integer = 1)
            CharPos += nMove
        End Sub

    Public Sub GetToken()
        Token = "" 'Inizializza
        TokType = Tok_Types.NONE

        If (CharPos > Source.Length) Then 'se arrivati alla fine della stringa
            TokType = Tok_Types.EOP
            Token = ControlChars.NullChar
            Exit Sub
        End If

        'Skip over white-spaces
        While CharPos <= Source.Length AndAlso IsWhite(Source(CharPos - 1))
            INC()
            If CharPos > Source.Length Then
                TokType = Tok_Types.EOL
                Exit Sub
            End If
        End While

        'Skip over Line Breaks
        If CharPos <= Source.Length AndAlso Source(CharPos - 1) = ControlChars.Cr Then
            INC(2)
            Token = ControlChars.Cr
            TokType = Tok_Types.EOL
            Exit Sub
        End If

        'Gestione speciale per "End"
        If CharPos + 2 <= Source.Length Then
            Dim possibleEnd = Source.Substring(CharPos - 1, Math.Min(3, Source.Length - (CharPos - 1))).ToLower()
            If possibleEnd = "end" Then
                Token = "End"
                INC(3)

                ' Prendi gli spazi dopo "End"
                While CharPos <= Source.Length AndAlso IsWhite(Source(CharPos - 1))
                    INC()
                End While

                ' Raccogli la parola che segue (Sub, Function, Module, etc.)
                Dim followingWord = New System.Text.StringBuilder()
                While CharPos <= Source.Length AndAlso
                  (IsAlpha(Source(CharPos - 1)) OrElse
                   Source(CharPos - 1) = "_"c)
                    followingWord.Append(Source(CharPos - 1))
                    INC()
                End While

                ' Se c'è una parola dopo "End", aggiungila al token
                If followingWord.Length > 0 Then
                    Token &= " " & followingWord.ToString()
                End If

                TokType = Tok_Types.LSTRING
                Exit Sub
            End If
        End If

        'Check for Delimiters
        If IsDelim(Source(CharPos - 1)) Then
            Token += Source(CharPos - 1)
            INC()
            If (Token = "(") Then
                TokType = Tok_Types.LPARM
            ElseIf (Token = ")") Then
                TokType = Tok_Types.RPARM
            Else
                TokType = Tok_Types.DEL
            End If
            Exit Sub
        End If

        'Gestione variabili con $
        If Source(CharPos - 1) = "$"c Then
            Token = "$"
            INC()
            If CharPos <= Source.Length AndAlso IsAlpha(Source(CharPos - 1)) Then
                While CharPos <= Source.Length AndAlso Not IsDelim(Source(CharPos - 1))
                    Token += Source(CharPos - 1)
                    INC()
                End While
            End If
            TokType = Tok_Types.LVARIABLE
            Exit Sub
        End If

        'Gestione parole e identificatori
        If IsAlpha(Source(CharPos - 1)) Then
            While CharPos <= Source.Length AndAlso Not IsDelim(Source(CharPos - 1))
                Token += Source(CharPos - 1)
                INC()
                TokType = Tok_Types.LSTRING
            End While
            Exit Sub
        End If

        'Gestione numeri
        If IsDigit(Source(CharPos - 1)) Then
            While CharPos <= Source.Length AndAlso Not IsDelim(Source(CharPos - 1))
                Token += Source(CharPos - 1)
                If CharPos > Source.Length Then Exit Sub
                INC()
            End While
            TokType = Tok_Types.DIGIT
            Exit Sub
        End If

        'Gestione stringhe tra virgolette
        If Source(CharPos - 1) = """"c Then
            INC()
            While CharPos <= Source.Length AndAlso
              Source(CharPos - 1) <> """"c AndAlso
              Source(CharPos - 1) <> ControlChars.Cr
                Token += Source(CharPos - 1)
                INC()
            End While
            If CharPos <= Source.Length AndAlso Source(CharPos - 1) = ControlChars.Cr Then
                Token = ControlChars.NullChar
                TokType = Tok_Types.EOP
                Exit Sub
            End If
            INC()
            TokType = Tok_Types.QSTRING
            Exit Sub
        End If

        'Gestione commenti
        If Source(CharPos - 1) = "'"c Then
            While CharPos <= Source.Length AndAlso Source(CharPos - 1) <> ControlChars.Cr
                Token += Source(CharPos - 1)
                INC()
            End While
            TokType = Tok_Types.COMMENT
            Exit Sub
        End If

        'Se arriviamo qui, c'è un errore
        Token = "PHASE_ERROR"
        TokType = Tok_Types.EOP
    End Sub

    Public Function Parse() As List(Of Parsed_Code)
        Dim Parsed As New List(Of Parsed_Code) 'Inizializza il risultato
        CharPos = 1 'partenza
        Do
            GetToken() 'estrai i token
            Parsed.Add(New Parsed_Code With {
                .Token = Token,
                .Symbol = GetStrToken(TokType),
                .Symbol_ID = TokType
            })
        Loop Until (TokType = Tok_Types.EOP)

        Return Parsed
    End Function


End Class
