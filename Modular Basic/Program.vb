Module Program
    Public Sub Main()
        ' Inizializza i componenti statici
        _CodeGen = New System.Text.StringBuilder()
        _LibGen = New System.Text.StringBuilder()
        _MemoryManager = New MemoryManager()
        _SymbolTable = New SymbolTable()

        ' Codice BASIC da compilare
        Const TestCode As String = "
         sub main()
          dim i as integer
          i=sqrt(10)+2
         end sub
        "

        ' Inizializza il parser e compila
        Try
            Dim parser As New VBParser(TestCode)
            tokens = parser.Parse()

            ' Mostra i token
            Console.WriteLine("=== Tokens Analizzati ===" & vbNewLine)
            For Each token In tokens
                '       Console.WriteLine($"Token: {token.Token.PadRight(20)} Type: {token.Symbol}")
            Next
            Dim codeGen As New CodeGenerator()
            codeGen.GenerateCode()


            ' Mostra il codice assembly generato
            Console.WriteLine(vbNewLine & "=== Codice Assembly Generato ===" & vbNewLine)
            Console.WriteLine(_CodeGen.ToString())

            Console.WriteLine(vbNewLine & "=== Librerie Assembly Generate ===" & vbNewLine)
            '  Console.WriteLine(_LibGen.ToString())

        Catch ex As Exception
            Console.WriteLine($"Errore durante la compilazione: {ex.Message}")
            Console.WriteLine($"StackTrace: {ex.StackTrace}")
        End Try

        Console.WriteLine(vbNewLine & "Premi un tasto per terminare...")
        Console.ReadKey()
    End Sub

End Module