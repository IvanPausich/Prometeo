
Public Class ExpressionParser
    Private _LabelCounter As Integer = 0

    ' Priorità operatori
    Private ReadOnly OperatorPriority As New Dictionary(Of String, Integer) From {
        {"^", 4},
        {"*", 3}, {"/", 3}, {"Mod", 3},
        {"+", 2}, {"-", 2},
        {"&", 1},  ' Concatenazione stringhe
        {"=", 0}, {"<>", 0}, {">", 0}, {"<", 0}, {">=", 0}, {"<=", 0}
    }

    Public Class ExpressionNode
        Public Value As String          ' Valore o operatore
        Public NodeType As String       ' "OPERATOR", "VARIABLE", "LITERAL"
        Public DataType As String       ' INTEGER, SINGLE, STRING, etc.
        Public Left As ExpressionNode   ' Operando sinistro
        Public Right As ExpressionNode  ' Operando destro
        Public TempVar As String        ' Variable temporanea per risultato
    End Class


End Class