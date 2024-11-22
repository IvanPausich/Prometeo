Imports System.Text.RegularExpressions

Public Class Compiler

    Private Class Label
        Public Name As String
        Public Offset As Integer

        Public Sub New(name As String, offset As Integer)
            Me.Name = name
            Me.Offset = offset
        End Sub
    End Class

    Private Class _LabelSet
        Private ReadOnly _labels As Dictionary(Of String, Label)

        Public Sub New()
            _labels = New Dictionary(Of String, Label)(StringComparer.OrdinalIgnoreCase)
        End Sub

        Public Sub AddLabel(name As String)
            If Not _labels.ContainsKey(name) Then
                _labels.Add(name, New Label(name, 0))
            End If
        End Sub

        Public Sub UpdateLabelOffset(name As String, offset As Integer)
            If _labels.ContainsKey(name) Then
                _labels(name).Offset = offset
            Else
                Console.WriteLine($"Label non trovata: {name}")
            End If
        End Sub

        Public Function GetLabelOffset(name As String) As Integer
            If _labels.ContainsKey(name) Then
                Return _labels(name).Offset
            Else
                Console.WriteLine($"Label non trovata: {name}")
            End If
        End Function

        Public Function ContainsLabel(name As String) As Boolean
            Return _labels.ContainsKey(name)
        End Function
    End Class

    Private labelSet As New _LabelSet()

    Private Parser As New Parser
    Private syntax As New Syntax()

    Public Sub Compile(code As String)
        Dim righe() As String = SplitCode(UCase(code))
        Dim parsed() As ParsedInstruction
        Dim isValid As Boolean
        syntax.Initializze_InstrList()
        ReDim parsed(UBound(righe))
        Trova_Label(UCase(code))
        For i As Integer = 0 To UBound(parsed)
            parsed(i) = Parser.ParseInstruction(righe(i))
        Next
        Risolvi_Offset(parsed)
        For i As Integer = 0 To UBound(parsed)
            isValid = syntax.ValidateInstruction(parsed(i))
            If isValid Then
                ' Log dell'istruzione originale e dei dettagli parsati
                LogInstructionDetails(parsed(i))
                CallInstruction(parsed(i))
            End If
        Next
    End Sub

    Private Sub LogInstructionDetails(parsed As ParsedInstruction)
        Dim isValid As Boolean = syntax.ValidateInstruction(parsed)
        If isValid Then
            Console.WriteLine("Istruzione originale: " & parsed.Mnemonic)
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

    Private Sub Risolvi_Offset(parsed() As ParsedInstruction)
        Dim isValid As Boolean
        Initialize()

        For i As Integer = 0 To UBound(parsed)
            isValid = syntax.ValidateInstruction(parsed(i))
            If isValid Then
                Dim pars As ParsedInstruction = DeepCopyParsedInstruction(parsed(i))
                If parsed(i).Mnemonic <> "DATA" AndAlso parsed(i).Mnemonic <> "RESDATA" Then
                    If Not String.IsNullOrEmpty(pars.Label) Then
                        Console.WriteLine(pars.Label & " = " & UBound(Codebyte) + 1)

                        labelSet.UpdateLabelOffset(pars.Label, UBound(Codebyte) + 1)
                    End If

                    For u As Integer = 0 To pars.Operands.Count - 1
                        If pars.Operands(u).Type = ParamType.ParamRel Then
                            pars.Operands(u).Type = ParamType.ParamImm
                            Select Case pars.Operands(u).Size
                                Case ParamSize.Bits8
                                    Dim a As Byte = 0
                                    pars.Operands(u).Value = a
                                Case ParamSize.Bits16
                                    Dim a As Short = 0
                                    pars.Operands(u).Value = a
                                Case ParamSize.Bits32
                                    Dim a As Integer = 0
                                    pars.Operands(u).Value = a
                            End Select
                        ElseIf pars.Operands(u).Type = ParamType.ParamMem Then
                            Dim originalValue As String = pars.Operands(u).Value.ToString()
                            ' Dividi la stringa in componenti
                            Dim componenti As String() = originalValue.Split(New Char() {" "c, "+"c}, StringSplitOptions.RemoveEmptyEntries)
                            Dim strs As String
                            ' Cerca una label valida tra i componenti
                            For d As Integer = 0 To componenti.Length - 1
                                ' Sostituisci la label nel componente
                                If labelSet.ContainsLabel(componenti(d)) Then componenti(d) = 0

                                'Stop ' Ricostruisci la stringa originale con la nuova label
                            Next
                            ' Ricomponi la stringa mantenendo l'ordine originale
                            For d As Integer = componenti.Length - 1 To 0 Step -1
                                strs = If(strs = "", componenti(d), strs & "+" & componenti(d))
                            Next
                            pars.Operands(u).Value = strs
                        End If
                    Next
                    CallInstruction(pars)
                End If
            End If
        Next
        For i As Integer = 0 To UBound(parsed)
            isValid = syntax.ValidateInstruction(parsed(i))
            If isValid Then
                If parsed(i).Mnemonic = "DATA" Or parsed(i).Mnemonic = "RESDATA" Then
                    If Not String.IsNullOrEmpty(parsed(i).Label) Then
                        labelSet.UpdateLabelOffset(parsed(i).Label, UBound(Codebyte) + UBound(DataByte))
                    End If
                    CallInstruction(parsed(i))
                End If
            End If
        Next
        Initialize()
        REplace_Offset(parsed)
    End Sub

    Private Sub Replace_Offset(parsed() As ParsedInstruction)
        Dim isValid As Boolean
        For i As Integer = 0 To UBound(parsed)
            isValid = syntax.ValidateInstruction(parsed(i))
            If isValid Then
                For u As Integer = 0 To parsed(i).Operands.Count - 1
                    Dim operand = parsed(i).Operands(u)
                    If operand.Type = ParamType.ParamRel Then
                        operand.Type = ParamType.ParamImm
                        Dim labelOffset = labelSet.GetLabelOffset(operand.Value.ToString())
                        Select Case operand.Qualifier
                            Case "SHORT"
                                operand.Value = CInt(labelOffset - UBound(Codebyte))
                            Case "NEAR"
                                operand.Value = CInt(labelOffset - UBound(Codebyte))
                            Case "FAR"
                                operand.Value = labelOffset
                            Case Else
                                ' Gestione di casi non previsti
                                Console.WriteLine($"Qualifier non riconosciuto: {operand.Qualifier}")
                        End Select
                    ElseIf operand.Type = ParamType.ParamMem Then

                        Dim originalValue As String = operand.Value.ToString()
                        ' Dividi la stringa in componenti
                        Dim componenti As String() = originalValue.Split(New Char() {" "c, "+"c}, StringSplitOptions.RemoveEmptyEntries)
                        Dim strs As String
                        ' Cerca una label valida tra i componenti
                        For d As Integer = 0 To componenti.Length - 1
                            ' Sostituisci la label nel componente
                            If labelSet.ContainsLabel(componenti(d)) Then
                                Dim offset = labelSet.GetLabelOffset(componenti(d))
                                componenti(d) = offset
                            End If
                            ' Ricostruisci la stringa originale con la nuova label
                        Next
                        ' Ricomponi la stringa mantenendo l'ordine originale
                        For d As Integer = componenti.Length - 1 To 0 Step -1
                            strs = If(strs = "", componenti(d), strs & "+" & componenti(d))
                        Next
                        operand.Value = strs
                    End If
                Next
                CallInstruction(parsed(i))
            End If
        Next
    End Sub

    Private Function DeepCopyParsedInstruction(original As ParsedInstruction) As ParsedInstruction
        Dim copy As New ParsedInstruction()
        copy.Label = original.Label
        copy.Mnemonic = original.Mnemonic
        copy.Comment = original.Comment

        ' Copia dei prefissi
        copy.Prefixes = New List(Of Byte)(original.Prefixes)

        ' Copia profonda degli operandi
        copy.Operands = New List(Of ParsedOperand)()
        For Each op In original.Operands
            Dim opCopy As New ParsedOperand()
            opCopy.Type = op.Type
            opCopy.Value = op.Value ' Nota: potrebbe essere necessario un clone più profondo se Value è un oggetto complesso
            opCopy.Size = op.Size
            opCopy.Qualifier = op.Qualifier
            copy.Operands.Add(opCopy)
        Next

        Return copy
    End Function

    Private Sub Trova_Label(code As String)
        Dim righe() As String = code.Split(New String() {vbCrLf, vbLf, vbCr}, StringSplitOptions.RemoveEmptyEntries)
        Dim parsed As ParsedInstruction
        Dim isValid As Boolean
        For i As Integer = 0 To UBound(righe)
            parsed = Parser.ParseInstruction(righe(i))
            isValid = syntax.ValidateInstruction(parsed)
            If isValid Then
                If Not String.IsNullOrEmpty(parsed.Label) Then
                    labelSet.AddLabel(parsed.Label)
                End If
            End If
        Next
    End Sub

    Public Function SplitCode(code As String) As String()
        ' Utilizziamo RegEx per gestire diversi tipi di line endings, rimuovere commenti di fine riga,
        ' e catturare solo il contenuto effettivo di ogni riga
        Dim linePattern As String = "^\s*([^'\r\n]+?)\s*(?:'.*)?(?:\r\n|\n|\r|$)"
        Dim matches As MatchCollection = Regex.Matches(code, linePattern, RegexOptions.Multiline)

        ' Convertiamo i match in un array di stringhe, rimuovendo le righe vuote
        Dim lines As List(Of String) = matches.Cast(Of Match)() _
                                              .Select(Function(m) m.Groups(1).Value.Trim()) _
                                              .Where(Function(line) Not String.IsNullOrWhiteSpace(line)) _
                                              .ToList()

        ' Gestiamo le righe di continuazione (se applicabile al tuo linguaggio)
        Dim result As New List(Of String)
        Dim currentLine As String = ""

        For Each line In lines
            If line.EndsWith("_") Then
                ' Rimuoviamo il carattere di continuazione e aggiungiamo alla riga corrente
                currentLine += line.TrimEnd("_"c) & " "
            Else
                ' Aggiungiamo la riga corrente (se non vuota) e iniziamo una nuova riga
                If Not String.IsNullOrEmpty(currentLine) Then
                    result.Add((currentLine & line).Trim())
                    currentLine = ""
                Else
                    result.Add(line.Trim())
                End If
            End If
        Next

        ' Aggiungiamo l'ultima riga se c'è una riga di continuazione alla fine
        If Not String.IsNullOrEmpty(currentLine) Then
            result.Add(currentLine.Trim())
        End If

        Return result.ToArray()
    End Function

    Private Sub CallInstruction(parsed As ParsedInstruction)
        Console.WriteLine(parsed.Mnemonic)
        Select Case parsed.Mnemonic
            Case "DATA"
                datatobin(parsed)
            Case "RESDATA"
                resdatatobin(parsed)
            Case "LIA"
                liatobin(parsed)
            Case "AAA"
                aaatobin()
            Case "AAD"
                aadtobin()
            Case "AAM"
                aamtobin()
            Case "AAS"
                aastobin()
            Case "ADC"
                adctobin(parsed)
            Case "ADD"
                addtobin(parsed)
            Case "AND"
                andtobin(parsed)
            Case "BOUND"
                boundtobin(parsed)
            Case "BSF"
                bsftobin(parsed)
            Case "BRS"
                bsrtobin(parsed)
            Case "BSWAP"
                bswaptobin(parsed)
            Case "BT"
                bttobin(parsed)
            Case "BTC"
                btctobin(parsed)
            Case "BTR"
                btrtobin(parsed)
            Case "BTS"
                btstobin(parsed)
            Case "CALL"
                CALLtobin(parsed)
            Case "CBW"
                cbwtobin()
            Case "CWDE"
                cwdetobin()
            Case "CWD"
                cwdtobin()
            Case "CDQ"
                cdqtobin()
            Case "CLC"
                clctobin()
            Case "CLD"
                cldtobin()
            Case "CLI"
                clitobin()
            Case "CMC"
                cmctobin()
            Case "CMP"
                cmptobin(parsed)
            Case "CMPSB"
                cmpsbtobin()
            Case "CMPSW"
                cmpswtobin()
            Case "CMPSD"
                cmpsdtobin()
            Case "INSB"
                insbtobin()
            Case "INSW"
                inswtobin()
            Case "INSD"
                insdtobin()
            Case "OUTSB"
                outsbtobin()
            Case "OUTSW"
                outswtobin()
            Case "OUTSD"
                outsdtobin()
            Case "CMPXCHG"
                cmpxchgtobin(parsed)
            Case "DAA"
                daatobin()
            Case "DAS"
                dastobin()
            Case "DEC"
                dectobin(parsed)
            Case "DIV"
                divtobin(parsed)
            Case "HLT"
                hlttobin()
            Case "IDIV"
                idivtobin(parsed)
            Case "IMUL"
                imultobin(parsed)
            Case "INC"
                inctobin(parsed)
            Case "IN"
                intobin(parsed)
            Case "OUT"
                outtobin(parsed)
            Case "JCXZ"
                jcxztobin(parsed)
            Case "JECXZ"
                jecxztobin(parsed)
            Case "JMP"
                JMPtobin(parsed)
            Case "LAHF"
                lahftobin()
            Case "LEA"
                leatobin(parsed)
            Case "LODSB"
                lodsbtobin()
            Case "LODSW"
                lodswtobin()
            Case "LODSD"
                lodsdtobin()
            Case "LOOP"
                looptobin(parsed)
            Case "LOOPE", "LOOPZ"
                loopeztobin(parsed)
            Case "LOOPNE", "LOOPNZ"
                loopneztobin(parsed)
            Case "MOV"
                movtobin(parsed)
            Case "MOVSB"
                movsbtobin()
            Case "MOVSW"
                movswtobin()
            Case "MOVSD"
                movsdtobin()
            Case "MOVSX"
                movsxtobin(parsed)
            Case "MOVZX"
                movzxtobin(parsed)
            Case "MUL"
                multobin(parsed)
            Case "NEG"
                negtobin(parsed)
            Case "NOP"
                noptobin()
            Case "NOT"
                nottobin(parsed)
            Case "OR"
                ortobin(parsed)
            Case "POP"
                poptobin(parsed)
            Case "POPA"
                popatobin()
            Case "POPAD"
                popadtobin()
            Case "POPF"
                popftobin()
            Case "POPFD"
                popfdtobin()
            Case "PUSH"
                pushtobin(parsed)
            Case "PUSHA"
                pushatobin()
            Case "PUSHAD"
                pushadtobin()
            Case "PUSHF"
                pushftobin()
            Case "PUSHFD"
                pushfdtobin()
            Case "RCL"
                rcltobin(parsed)
            Case "RCR"
                rcrtobin(parsed)
            Case "RET"
                rettobin(parsed)
            Case "IRET"
                irettobin()
            Case "ROL"
                roltobin(parsed)
            Case "ROR"
                rortobin(parsed)
            Case "SAHF"
                sahftobin()
            Case "SHL"
                shltobin(parsed)
            Case "SAR"
                sartobin(parsed)
            Case "SBB"
                sbbtobin(parsed)
            Case "SCASB"
                scasbtobin()
            Case "SCASW"
                scaswtobin()
            Case "SCASD"
                scasdtobin()
            Case "SHLD"
                shldtobin(parsed)
            Case "SHR"
                shrtobin(parsed)
            Case "SHRD"
                shrdtobin(parsed)
            Case "STC"
                stctobin()
            Case "STD"
                stdtobin()
            Case "STI"
                stitobin()
            Case "STOSB"
                stosbtobin()
            Case "STOSW"
                stoswtobin()
            Case "STOSD"
                stosdtobin()
            Case "SUB"
                subtobin(parsed)
            Case "TEST"
                testtobin(parsed)
            Case "XADD"
                xaddtobin(parsed)
            Case "XCHG"
                xchgtobin(parsed)
            Case "XLATB"
                xlatbtobin()
            Case "XOR"
                xortobin(parsed)
            Case "REP", "REPE"
                repetobin()
            Case "REPNE"
                repnetobin()
            Case "FADD"
                faddtobin(parsed)
            Case "FCOM"
                fcomtobin(parsed)
            Case "FCOMP"
                fcomptobin(parsed)
            Case "FCOMI"
                fcomitobin(parsed)
            Case "FCOMIP"
                fcomiptobin(parsed)
            Case "FDIV"
                fdivtobin(parsed)
            Case "FLD"
                fldtobin(parsed)
            Case "FMUL"
                fmultobin(parsed)
            Case "FSQRT"
                fsqrttobin()
            Case "FSCALE"
                fscaletobin()
            Case "FST"
                fsttobin(parsed)
            Case "FSTP"
                fstptobin(parsed)
            Case "FSUB"
                fsubtobin(parsed)
            Case "FXCH"
                fxchtobin(parsed)
            Case "ADDPD"
                addpdtobin(parsed)
            Case "ANDPD"
                andpdtobin(parsed)
            Case "CMPPD"
                cmppdtobin(parsed)
            Case "CVTSI2SD"
                cvtsi2sdtobin(parsed)
            Case "CVTPD2PS"
                cvtpd2pstobin(parsed)
            Case "CVTPS2PD"
                cvtps2pdtobin(parsed)
            Case "DIVPD"
                divpdtobin(parsed)
            Case "MAXPD"
                maxpdtobin(parsed)
            Case "MINPD"
                minpdtobin(parsed)
            Case "MOVAPD"
                movapdtobin(parsed)
            Case "MOVUPD"
                movupdtobin(parsed)
            Case "MULPD"
                mulpdtobin(parsed)
            Case "ORPD"
                orpdtobin(parsed)
            Case "SHUFPD"
                shufpdtobin(parsed)
            Case "SUBPD"
                subpdtobin(parsed)
            Case "UNPCKHPD"
                unpckhpdtobin(parsed)
            Case "UNPCKLPD"
                unpcklpdtobin(parsed)
            Case "XORPD"
                xorpdtobin(parsed)
            Case "PADDD"
                padddtobin(parsed)
            Case "PMULLW"
                pmullwtobin(parsed)
            Case "PSUBD"
                psubdtobin(parsed)
            Case "ADDPS"
                addpstobin(parsed)
            Case "ANDPS"
                andpstobin(parsed)
            Case "CMPPS"
                cmppstobin(parsed)
            Case "CVTSI2SS"
                cvtsi2sstobin(parsed)
            Case "DIVPS"
                divpstobin(parsed)
            Case "MAXPS"
                maxpstobin(parsed)
            Case "MINPS"
                minpstobin(parsed)
            Case "MOVAPS"
                movapstobin(parsed)
            Case "MOVUPS"
                movupstobin(parsed)
            Case "MULPS"
                mulpstobin(parsed)
            Case "ORPS"
                orpstobin(parsed)
            Case "SHUFPS"
                shufpstobin(parsed)
            Case "SUBPS"
                subpstobin(parsed)
            Case "UNPCKHPS"
                unpckhpstobin(parsed)
            Case "UNPCKLPS"
                unpcklpstobin(parsed)
            Case "XORPS"
                xorpstobin(parsed)
            Case "EXTRACTPS"
                extractpstobin(parsed)
            Case "INSERTPS"
                insertpstobin(parsed)
        End Select
        If parsed.Mnemonic <> "JMP" And parsed.Mnemonic <> "JCXZ" And parsed.Mnemonic <> "JECXZ" And Left(parsed.Mnemonic, 1) = "J" Then

            jtobin(parsed, ConditionOffset(Mid(parsed.Mnemonic, 2)))
        ElseIf Len(parsed.Mnemonic) > 4 And Left(parsed.Mnemonic, 4) = "CMOV" Then
            cmovtobin(parsed, ConditionOffset(Mid(parsed.Mnemonic, 5)))
        ElseIf Left(parsed.Mnemonic, 3) = "SET" Then
            settobin(parsed, ConditionOffset(Mid(parsed.Mnemonic, 4)))
        End If
        PrintByteArrayHex(Codebyte)

    End Sub
End Class