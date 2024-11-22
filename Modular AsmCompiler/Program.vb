
Imports System.Formats
Imports System.Text.RegularExpressions

Module CompilerDecompilerTest
    Private Compiler As New Compiler()
    Private Parser As New Parser
    Dim syntax As New Syntax()

    Sub Main(args As String())
        syntax.Initializze_InstrList()
        ParseAndPrint("LIA dword(12345)")
        ParseAndPrint("start: MOV EAX, dword(10)")
        ParseAndPrint("PUSH EAX")
        ParseAndPrint("CALL calculate")
        ParseAndPrint("ADD ESP, 4")
        ParseAndPrint("JMP end")
        ParseAndPrint("calculate: PUSH EBP")
        ParseAndPrint("MOV EBP, ESP")
        ParseAndPrint("MOV EAX, [EBP + 8]")
        ParseAndPrint("IMUL EAX, eax, 2")
        ParseAndPrint("POP EBP")
        ParseAndPrint("RET")
        ParseAndPrint("end: MOV EAX, dword(1)")
        TestComplexCode()

    End Sub

    Private Sub ParseAndPrint(instruction As String)
        Dim parsed As ParsedInstruction = Parser.ParseInstruction(instruction)
        Dim isValid As Boolean = syntax.ValidateInstruction(parsed)
        If isValid Then
            Console.WriteLine("Istruzione originale: " & instruction)
            Console.WriteLine("----------------------------")
            Console.WriteLine($"  Label: {If(String.IsNullOrEmpty(parsed.Label), "(none)", parsed.Label)}")
            Console.WriteLine($"  Mnemonic: {parsed.Mnemonic}")
            Console.WriteLine($"  Prefixes: {String.Join(", ", parsed.Prefixes.Select(Function(b) $"0x{b:X2}"))}")
            Console.WriteLine("  Operands:")
            For i As Integer = 0 To parsed.Operands.Count - 1
                Dim operand As ParsedOperand = parsed.Operands(i)
                Console.WriteLine($"    Operand {i + 1}:")
                Console.WriteLine($"      Type: {operand.Type}")
                Console.WriteLine($"      Value: {operand.Value}")
                Console.WriteLine($"      Size: {operand.Size}")
                Console.WriteLine($"      Qualifier: {If(String.IsNullOrEmpty(operand.Qualifier), "(none)", operand.Qualifier)}")
            Next
            Console.WriteLine($"  Comment: {If(String.IsNullOrEmpty(parsed.Comment), "(none)", parsed.Comment)}")
        Else
            Console.WriteLine("Istruzione non valida: " & instruction)
            Console.WriteLine("----------------------------")
            Console.WriteLine($"  Label: {If(String.IsNullOrEmpty(parsed.Label), "(none)", parsed.Label)}")
            Console.WriteLine($"  Mnemonic: {parsed.Mnemonic}")
            Console.WriteLine($"  Prefixes: {String.Join(", ", parsed.Prefixes.Select(Function(b) $"0x{b:X2}"))}")
            Console.WriteLine("  Operands:")
            For i As Integer = 0 To parsed.Operands.Count - 1
                Dim operand As ParsedOperand = parsed.Operands(i)
                Console.WriteLine($"    Operand {i + 1}:")
                Console.WriteLine($"      Type: {operand.Type}")
                Console.WriteLine($"      Value: {operand.Value}")
                Console.WriteLine($"      Size: {operand.Size}")
                Console.WriteLine($"      Qualifier: {If(String.IsNullOrEmpty(operand.Qualifier), "(none)", operand.Qualifier)}")
            Next
            Console.WriteLine($"  Comment: {If(String.IsNullOrEmpty(parsed.Comment), "(none)", parsed.Comment)}")

        End If
    End Sub


    Private Sub TestComplexCode()
        Console.WriteLine("Testing Complex Code")
        Console.WriteLine("====================")
        Dim code As String = "LIA dword(12345)" & vbCrLf &
            "start: MOV EAX, dword(10)" & vbCrLf &
    "PUSH EAX" & vbCrLf &
    "CALL near calculate" & vbCrLf &
    "ADD ESP, dword(4)" & vbCrLf &
      "Je short end" & vbCrLf &
  "JMP short end" & vbCrLf &
"calculate: PUSH EBP" & vbCrLf &
    "MOV EBP, ESP" & vbCrLf &
    "MOV EAX, [EBP + END]" & vbCrLf &
    "IMUL EAX, eax, 2" & vbCrLf &
      "JMP far end" & vbCrLf &
"POP EBP" & vbCrLf &
    "RET" & vbCrLf &
"end: MOV EAX, dword(1)"

        Compiler.Compile(code)
    End Sub

End Module