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
        Token = ""
        TokType = Tok_Types.NONE

        ' Check EOF
        If CharPos >= Source.Length Then
            TokType = Tok_Types.EOP
            Token = ControlChars.NullChar
            Exit Sub
        End If

        ' Skip whitespace
        While CharPos < Source.Length AndAlso IsWhitespace(Source(CharPos))
            CharPos += 1
        End While

        ' EOF after whitespace
        If CharPos >= Source.Length Then
            TokType = Tok_Types.EOP
            Token = ControlChars.NullChar
            Exit Sub
        End If

        ' Get current char
        Dim currentChar = Source(CharPos)
        CharPos += 1

        ' Handle line breaks
        If currentChar = vbCr OrElse currentChar = vbLf Then
            If currentChar = vbCr AndAlso CharPos < Source.Length AndAlso Source(CharPos) = vbLf Then
                CharPos += 1

            End If
            Token = ControlChars.NullChar
            TokType = Tok_Types.EOL
            Exit Sub
        End If

        ' Handle keywords and identifiers
        If Char.IsLetter(currentChar) OrElse currentChar = "_"c Then
            Token = currentChar
            While CharPos < Source.Length AndAlso
             (Char.IsLetterOrDigit(Source(CharPos)) OrElse Source(CharPos) = "_"c)
                Token += Source(CharPos)
                CharPos += 1

            End While

            TokType = Tok_Types.LSTRING
            Exit Sub
        End If

        ' Handle numbers
        If Char.IsDigit(currentChar) Then
            Token = currentChar
            'Stop
            While CharPos < Source.Length AndAlso
             (Char.IsDigit(Source(CharPos)) OrElse Source(CharPos) = "."c)
                Token += Source(CharPos)
                CharPos += 1

            End While
            TokType = Tok_Types.DIGIT
            Exit Sub
        End If

        ' Handle strings
        If currentChar = """"c Then
            While CharPos < Source.Length AndAlso Source(CharPos) <> """"c
                If Source(CharPos) = vbCr OrElse Source(CharPos) = vbLf Then
                    TokType = Tok_Types.EOP
                    Exit Sub
                End If
                Token += Source(CharPos)
                CharPos += 1
            End While
            CharPos += 1 ' Skip closing quote
            TokType = Tok_Types.QSTRING

            Exit Sub
        End If

        ' Handle comments
        If currentChar = "'"c Then
            While CharPos < Source.Length AndAlso
             Source(CharPos) <> vbCr AndAlso
             Source(CharPos) <> vbLf
                Token += Source(CharPos)
                CharPos += 1

            End While
            TokType = Tok_Types.COMMENT
            Exit Sub
        End If

        ' Handle operators and delimiters
        Select Case currentChar
            Case "(", ")", "+", "-", "*", "/", "=", ">", "<", ",", "."
                Token = currentChar
                TokType = If(currentChar = "(", Tok_Types.LPARM,
                    If(currentChar = ")", Tok_Types.RPARM, Tok_Types.DEL))
                'CharPos += 1

                Exit Sub
        End Select
    End Sub

    Private Sub SkipWhitespace()
        While CharPos < Source.Length AndAlso IsWhitespace(Source(CharPos))
            CharPos += 1
        End While
    End Sub

    Private Function ReadIdentifier() As String
        Dim id As String = ""
        While CharPos < Source.Length AndAlso
         (Char.IsLetterOrDigit(Source(CharPos)) OrElse Source(CharPos) = "_"c)
            id += Source(CharPos)
            CharPos += 1
        End While
        Return id
    End Function

    Private Function IsWhitespace(c As Char) As Boolean
        Return c = " "c OrElse c = vbTab
    End Function

    Public Function Parse() As List(Of Parsed_Code)
        Dim Parsed As New List(Of Parsed_Code) 'Inizializza il risultato
        CharPos = 0 'partenza
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
