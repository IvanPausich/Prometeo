Module Program
    Public Sub Main()
        ' Inizializza i componenti statici
        ' Codice BASIC da compilare
        Const TestCode As String = "'test modulo
Module TestModule
    ' Dichiarazioni valide di variabili
    Private testVar As Integer = 100
    Public testString As String = ""Hello""
    Dim testArray(10) As Integer
    Dim x As Integer = 5, y As String = ""test"", z As Single = 3.14

    ' Dichiarazioni multiple in una riga
    Private a As Integer, b As String, c(5) As Single

    ' Test di funzione
    Public Function TestFunction(ByVal param As Integer) As Integer
        ' Dichiarazione valida dentro funzione
        Dim localVar As Integer = 20
        
        ' Test dichiarazione array
        Dim arr(50) As String
        
        ' Test dichiarazione non valida (dovrebbe dare errore)
        'Public wrongVar As Integer
        
        Return localVar + param
    End Function
     dim sclas as testclass
    ' Test di dichiarazioni non valide
    'Dim invalidVar Integer  ' Manca As
    'Dim 123var As String    ' Nome non valido
    'Dim test As Unknown     ' Tipo non valido
    
    ' Test di valori fuori range
    'Dim outOfRange As Byte = 300  ' Dovrebbe dare errore
    
    ' Test di Sub
    Private Sub TestSub()
        Dim testVar As Integer = 10
        
        ' Test annidamento
        If testVar > 0 Then
            Dim nestedVar As Integer = 5
            For i As Integer = 0 To 10
                Dim loopVar As String = ""test""
            Next
        End If
    End Sub
End Module

'test classe
Public Class TestClass
    Private classVar As Integer
    Public staticVar As String
    
    ' Costruttore
    Public Sub New()
        Dim constructorVar As Integer = 0
    End Sub
    
    ' Metodo con array
    Public Sub ProcessArray()
        Dim data(100) As Integer
        Dim matrix(10, 10) As Single
        
        ' Test di cicli e condizioni
        For i As Integer = 0 To 10
            If i Mod 2 = 0 Then
                data(i) = i * 2
            Else
                data(i) = i
            End If
        Next
    End Sub
End Class
"

        Dim parser As New VBParser(TestCode)
        Dim tokens = parser.Parse()
        ' Mostra i token
        Console.WriteLine("=== Tokens Analizzati ===" & vbNewLine)
        For Each token In tokens
            Console.WriteLine($"Token: {token.Token.PadRight(20)} Type: {token.Symbol}")
        Next

        Dim lexer As New Lexer()
        Try
            lexer.Inizializza(TestCode)
            lexer.Lex()
            Console.WriteLine("Analisi completata con successo")
        Catch ex As Exception
            Console.WriteLine($"Errore nell'analisi: {ex.Message}")
        End Try



        Console.WriteLine(vbNewLine & "Premi un tasto per terminare...")
        Console.ReadKey()
    End Sub

End Module