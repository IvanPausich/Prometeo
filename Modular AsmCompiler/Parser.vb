Imports System.ComponentModel
Imports System.Text.RegularExpressions

Public Class Parser

    Public Function ParseInstruction(instruction As String) As Object
        instruction = instruction.Trim()
        Dim result As New ParsedInstruction With {
        .Operands = New List(Of ParsedOperand)()
    }

        ' Estrai il commento
        Dim commentIndex As Integer = instruction.IndexOf("'")
        If commentIndex >= 0 Then
            result.Comment = instruction.Substring(commentIndex + 1).Trim()
            instruction = instruction.Substring(0, commentIndex).Trim()
        End If

        ' Estrai la label
        Dim labelMatch As Match = Regex.Match(instruction, "^(\w+):")
        If labelMatch.Success Then
            result.Label = labelMatch.Groups(1).Value
            instruction = instruction.Substring(labelMatch.Length).Trim()
        End If

        ' Analizza i prefissi
        Dim prefixesResult = ParseInstructionPrefixes(instruction)
        result.Prefixes = prefixesResult.Item2
        instruction = prefixesResult.Item1

        ' Dividi l'istruzione in mnemonic e operandi
        Dim parts As String() = instruction.Split(New Char() {" "c, vbTab}, 2)
        If parts.Length > 0 Then
            result.Mnemonic = parts(0).ToUpper()
        End If

        ' Analizza gli operandi
        If parts.Length > 1 Then
            Dim operandsString As String = parts(1)
            Dim operands = SplitOperands(operandsString)
            For Each operand In operands
                result.Operands.Add(ParseOperand(operand))
            Next
        End If

        Return result
    End Function


    Private Function SplitOperands(operandsString As String) As List(Of String)
        Dim operands As New List(Of String)
        Dim currentOperand As String = ""
        Dim inQuotes As Boolean = False
        Dim inBrackets As Integer = 0

        For Each c As Char In operandsString
            Select Case c
                Case """"c
                    inQuotes = Not inQuotes
                    currentOperand += c
                Case "["c
                    inBrackets += 1
                    currentOperand += c
                Case "]"c
                    inBrackets -= 1
                    currentOperand += c
                Case ","c
                    If Not inQuotes AndAlso inBrackets = 0 Then
                        operands.Add(currentOperand.Trim())
                        currentOperand = ""
                    Else
                        currentOperand += c
                    End If
                Case Else
                    currentOperand += c
            End Select
        Next

        If currentOperand.Trim() <> "" Then
            operands.Add(currentOperand.Trim())
        End If

        Return operands
    End Function

    Private Function ParseOperand(operand As String) As ParsedOperand
        Dim result As New ParsedOperand()

        ' Gestione degli operandi composti
        Dim composedOperandMatch = Regex.Match(operand, "^(SHORT|NEAR|FAR|BYTE|WORD|DWORD|QWORD|TBYTE|DQWORD)\s+(.+)$", RegexOptions.IgnoreCase)
        If composedOperandMatch.Success Then
            result.Qualifier = composedOperandMatch.Groups(1).Value.ToUpper()
            operand = composedOperandMatch.Groups(2).Value.Trim()
        End If

        If AsmOpcode.IsFPUReg(operand) Then
            result.Type = ParamType.ParamSTX
            result.Value = AsmOpcode.FPURegStrToNum(operand)
            result.Size = ParamSize.Bits80
        ElseIf IsTextOperand(operand) Then
            result.Type = ParamType.ParamImm
            result.Value = ParseTextOperand(operand)
            result.Size = ParamSize.BitsUnknown
        ElseIf AsmOpcode.IsRegister(operand) Then
            result.Type = ParamType.ParamReg
            result.Value = AsmOpcode.RegStrToReg(operand)
            result.Size = AsmOpcode.RegisterSize(result.Value)
        ElseIf AsmOpcode.IsMMReg(operand) Then
            result.Type = ParamType.ParamMM
            result.Value = AsmOpcode.MMRegStrToNum(operand)
            result.Size = If(operand.StartsWith("XMM", StringComparison.OrdinalIgnoreCase), ParamSize.Bits128, ParamSize.Bits64)
        ElseIf operand.StartsWith("[") AndAlso operand.EndsWith("]") Then
            result.Type = ParamType.ParamMem
            result.Value = ParseMemoryOperand(operand.Trim("[]".ToCharArray()))
            result.Size = GetMemoryOperandSize(result.Qualifier)
        ElseIf IsImmediate(operand) Then
            result.Type = ParamType.ParamImm
            result.Value = ParseImmediate(operand)
            result.Size = GetImmediateSizeWithQualifier(result.Value, result.Qualifier)
        ElseIf IsLabel(operand) Then
            result.Type = ParamType.ParamRel
            result.Value = operand
            result.Size = If(result.Qualifier = "SHORT", ParamSize.Bits8, If(result.Qualifier = "NEAR", ParamSize.Bits16, ParamSize.Bits32))
        Else
            Throw New Exception($"Unrecognized operand type: {operand}")
        End If

        Return result
    End Function

    Private Function GetImmediateSizeWithQualifier(value As Object, qualifier As String) As ParamSize
        If Not String.IsNullOrEmpty(qualifier) Then
            Select Case qualifier.ToUpper()
                Case "BYTE" : Return ParamSize.Bits8
                Case "WORD" : Return ParamSize.Bits16
                Case "DWORD" : Return ParamSize.Bits32
            End Select
        End If

        ' Se non c'è un qualificatore o il qualificatore non è riconosciuto, usa la logica esistente
        Return GetImmediateSize(value)
    End Function

    Private Function ParseSTRegister(index As String) As ASMFPURegisters
        Dim i As Integer
        If Integer.TryParse(index, i) AndAlso i >= 0 AndAlso i <= 7 Then
            Return CType(i, ASMFPURegisters)
        Else
            Throw New ArgumentException($"Invalid ST register index: {index}")
        End If
    End Function

    Private Function ParseMemoryOperand(operand As String) As MemoryOperand
        Dim result As New MemoryOperand()

        ' Dividi l'operando in componenti
        Dim components = SplitMemoryOperand(operand)

        For Each component In components
            If AsmOpcode.IsRegister(component) Then
                If String.IsNullOrEmpty(result.BaseRegister) Then
                    result.BaseRegister = component
                ElseIf String.IsNullOrEmpty(result.IndexRegister) Then
                    result.IndexRegister = component
                End If
            ElseIf component.Contains("*") Then
                Dim parts = component.Split("*"c)
                result.IndexRegister = parts(0)
                result.Scale = Integer.Parse(parts(1))
            ElseIf component.StartsWith("&H") Then
                result.Displacement = Convert.ToInt32(component.Substring(2), 16)
            ElseIf Integer.TryParse(component, result.Displacement) Then
                ' Displacement is already parsed
            ElseIf IsLabel(component) Then
                result.Label = component
            End If
        Next

        ' Gestisci il segno dello spostamento
        If operand.Contains("-") Then
            result.Displacement = -Math.Abs(result.Displacement)
        End If

        Return result
    End Function

    Private Function SplitMemoryOperand(operand As String) As List(Of String)
        Dim components As New List(Of String)
        Dim currentComponent As String = ""
        Dim inBrackets As Integer = 0

        For Each c As Char In operand
            Select Case c
                Case "["c
                    inBrackets += 1
                    If inBrackets > 1 Then currentComponent += c
                Case "]"c
                    inBrackets -= 1
                    If inBrackets > 0 Then currentComponent += c
                Case "+"c, "-"c
                    If inBrackets = 0 AndAlso currentComponent.Length > 0 Then
                        components.Add(currentComponent.Trim())
                        currentComponent = ""
                    Else
                        currentComponent += c
                    End If
                Case Else
                    currentComponent += c
            End Select
        Next

        If currentComponent.Length > 0 Then
            components.Add(currentComponent.Trim())
        End If

        Return components
    End Function

    Private Function IsTextOperand(operand As String) As Boolean
        ' Verifica se l'operando è racchiuso tra virgolette singole o doppie
        Return (operand.StartsWith("""") AndAlso operand.EndsWith("""")) OrElse
               (operand.StartsWith("'") AndAlso operand.EndsWith("'"))
    End Function

    Private Function ParseTextOperand(operand As String) As String
        ' Rimuove le virgolette iniziali e finali
        Return operand.Trim(""""c, "'"c)
    End Function

    Private Function IsImmediate(operand As String) As Boolean
        ' Pattern per immediati con dimensione esplicita
        Dim explicitSizePattern As String = "^(Byte|Word|Dword)\s*\((.*?)\)$"

        ' Pattern per immediati senza dimensione esplicita
        Dim implicitSizePattern As String = "^(&H[0-9A-Fa-f]+|&B[01]+|-?\d+(\.\d+)?)$"

        Return Regex.IsMatch(operand, explicitSizePattern, RegexOptions.IgnoreCase) OrElse
           Regex.IsMatch(operand, implicitSizePattern)
    End Function

    Private Function ParseImmediate(operand As String) As Object
        ' Verifica se c'è una dimensione esplicita
        Dim explicitSizeMatch As Match = Regex.Match(operand, "^(Byte|Word|Dword)\s*\((.*?)\)$", RegexOptions.IgnoreCase)
        If explicitSizeMatch.Success Then
            Dim size As String = explicitSizeMatch.Groups(1).Value.ToLower()
            Dim value As String = explicitSizeMatch.Groups(2).Value
            Return ParseExplicitSizedImmediate(size, value)
        End If

        ' Se non c'è una dimensione esplicita, determina il tipo in base al valore
        If operand.StartsWith("&H") Then
            Return ParseHexImmediate(operand.Substring(2))
        ElseIf operand.StartsWith("&B") Then
            Return ParseBinaryImmediate(operand.Substring(2))
        ElseIf Long.TryParse(operand, Nothing) Then
            Return ParseNumericImmediate(operand)
        ElseIf operand.Contains(".") Then
            Return ParseNumericImmediate(operand)
        Else
            Return operand ' Potrebbe essere una label o un simbolo
        End If
    End Function

    Private Function ParseExplicitSizedImmediate(size As String, value As String) As Object
        Dim numericValue As Long
        If value.StartsWith("&H") Then
            numericValue = Convert.ToInt32(value.Substring(2), 16)
        ElseIf value.StartsWith("&B") Then
            numericValue = Convert.ToInt32(value.Substring(2), 2)
        ElseIf Long.TryParse(VALUE, numericValue) Then
            'Displacement Is already parsed
        Else
            Console.WriteLine("Invalid value for explicit sized immediate: " & value)
        End If

        Select Case size.ToLower()
            Case "byte"
                If numericValue >= Byte.MinValue AndAlso numericValue <= Byte.MaxValue Then
                    Return CByte(numericValue)
                ElseIf numericValue >= SByte.MinValue AndAlso numericValue <= SByte.MaxValue Then
                    Return CSByte(numericValue)
                End If
            Case "word"
                If numericValue >= Short.MinValue AndAlso numericValue <= Short.MaxValue Then
                    Return CShort(numericValue)
                ElseIf numericValue >= UShort.MinValue AndAlso numericValue <= UShort.MaxValue Then
                    Return CUShort(numericValue)
                End If
            Case "dword"
                If numericValue >= Integer.MinValue AndAlso numericValue <= Integer.MaxValue Then
                    Return CInt(numericValue)
                ElseIf numericValue >= UInteger.MinValue AndAlso numericValue <= UInteger.MaxValue Then
                    Return CUInt(numericValue)
                ElseIf numericValue >= Single.MinValue AndAlso numericValue <= Single.MaxValue Then
                    Return CSng(numericValue)
                End If
        End Select
        Stop
    End Function

    Private Function ParseHexImmediate(hexValue As String) As Object
        Dim numericValue As Integer = Convert.ToInt32(hexValue, 16)
        Return DetermineImmediateType(numericValue)
    End Function

    Private Function ParseBinaryImmediate(binaryValue As String) As Object
        Dim numericValue As Integer = Convert.ToInt32(binaryValue, 2)
        Return DetermineImmediateType(numericValue)
    End Function

    Private Function ParseNumericImmediate(numericString As String) As Object
        If numericString.Contains(".") Then
            Return Single.Parse(numericString)
        Else
            Dim numericValue As Long
            If Long.TryParse(numericString, numericValue) Then
                Return DetermineImmediateType(numericValue)
            Else
                Console.WriteLine("Invalid numeric immediate: " & numericString)
            End If
        End If
    End Function

    Private Function DetermineImmediateType(value As Object) As Object
        If value >= Byte.MinValue AndAlso value <= Byte.MaxValue Then
            Return CByte(value)
        ElseIf value >= SByte.MinValue AndAlso value <= SByte.MaxValue Then
            Return CSByte(value)
        ElseIf value >= UShort.MinValue AndAlso value <= UShort.MaxValue Then
            Return CUShort(value)
        ElseIf value >= Short.MinValue AndAlso value <= Short.MaxValue Then
            Return CShort(value)
        ElseIf value >= UInteger.MinValue AndAlso value <= UInteger.MaxValue Then
            Return CUInt(value)
        ElseIf value >= Integer.MinValue AndAlso value <= Integer.MaxValue Then
            Return CInt(value)
        ElseIf value >= Single.MinValue AndAlso value <= Single.MaxValue Then
            Return CSng(value)
        End If
        Stop
    End Function

    Private Function IsLabel(operand As String) As Boolean
        ' Miglioriamo la funzione IsLabel per riconoscere più tipi di label valide
        ' Una label valida può iniziare con una lettera o un underscore,
        ' seguito da lettere, numeri, underscore, o punti (per label locali)
        Return Regex.IsMatch(operand, "^[a-zA-Z_][\w.]*$")
    End Function

    Private Function GetMemoryOperandSize(sizeStr As String) As ParamSize
        Select Case UCase(sizeStr)
            Case "BYTE" : Return ParamSize.Bits8
            Case "WORD" : Return ParamSize.Bits16
            Case "DWORD" : Return ParamSize.Bits32
            Case "QWORD" : Return ParamSize.Bits64
            Case "TBYTE" : Return ParamSize.Bits80
            Case "DQWORD" : Return ParamSize.Bits128
            Case Else : Return ParamSize.BitsUnknown
        End Select
    End Function

    Private Function GetImmediateSize(value As Object) As ParamSize
        Select Case value.GetType()
            Case GetType(SByte), GetType(Byte) : Return ParamSize.Bits8
            Case GetType(Short), GetType(UShort) : Return ParamSize.Bits16
            Case GetType(Integer), GetType(UInteger), GetType(Single) : Return ParamSize.Bits32
            Case GetType(String) : Return ParamSize.BitsUnknown ' La dimensione dipende dalla lunghezza della stringa
            Case Else : Return ParamSize.BitsUnknown
        End Select
    End Function

    Private Function ParseInstructionPrefixes(instruction As String) As (String, List(Of Byte))
        Dim prefixes As New List(Of Byte)()
        Dim parts = instruction.Split(New Char() {" "c, vbTab}, StringSplitOptions.RemoveEmptyEntries)
        Dim i = 0

        While i < parts.Length
            Select Case parts(i).ToUpper()
                Case "REP"
                    prefixes.Add(&HF3)
                Case "REPE", "REPZ"
                    prefixes.Add(&HF3)
                Case "REPNE", "REPNZ"
                    prefixes.Add(&HF2)
                Case Else
                    ' Non è un prefisso, quindi abbiamo finito
                    Exit While
            End Select
            i += 1
        End While

        Return (String.Join(" ", parts.Skip(i)), prefixes)
    End Function


End Class