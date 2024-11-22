Module Cod_FIstruzioni

    'r\m, r\m (8-16-32)
    'acc, imm (8-16-32)
    'r\m, imm (8-16-32/imm8)
    Public Sub GruppoA_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, OpCode2 As Byte, CampoReg As Byte)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If (op1.Value = ASMRegisters.RegAL Or op1.Value = ASMRegisters.RegAX Or op1.Value = ASMRegisters.RegEAX) And op1.Size <> op2.Size Then 'controlla se si tratta di istruzione rapida con accumulatore 
                    OpCode2 = IIf(op1.Size = ParamSize.Bits8, OpCode2, SetBit(OpCode2, 0))
                    AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                    If op1.Size = ParamSize.Bits8 Then 'controlla la dimensione dell'immediato e lo carica
                        AddCodeByte(op2.Value)
                    ElseIf op1.Size = ParamSize.Bits16 Then
                        AddCodeWord(op2.Value)
                    Else
                        AddCodeDWord(op2.Value)
                    End If
                Else 'se non è una operazione rapida con acumulatore
                    Dim opcod As Byte = &H80
                    If op1.Size <> ParamSize.Bits8 Then opcod = SetBit(opcod, 0) 'setta il bit w se non è una operazione a 8bit
                    If op1.Size <> op2.Size And op2.Size = ParamSize.Bits8 Then opcod = SetBit(opcod, 1) 'setta il bit s se non è una operazione sign extend
                    AddCodeByte(opcod) 'carica l'opcode dell'istruzione
                    AddCodeByte(EncodeModRM(&H3, CampoReg, RegToBin(op1.Value)))
                    Select Case op2.Size 'carica l'immediato
                        Case ParamSize.Bits8
                            AddCodeByte(op2.Value)
                        Case ParamSize.Bits16
                            AddCodeWord(op2.Value)
                        Case ParamSize.Bits32
                            AddCodeDWord(op2.Value)
                    End Select
                End If
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                OpCode1 = IIf(op2.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                Dim opcod As Byte = &H80
                If op1.Size = ParamSize.BitsUnknown Then
                    If op2.Size <> ParamSize.Bits8 Then opcod = SetBit(opcod, 0) 'setta il bit w se non è una operazione a 8bit
                Else
                    If op1.Size <> ParamSize.Bits8 Then opcod = SetBit(opcod, 0) 'setta il bit w se non è una operazione a 8bit
                    If op1.Size <> op2.Size And op2.Size = ParamSize.Bits8 Then opcod = SetBit(opcod, 1) 'setta il bit s se non è una operazione sign extend
                End If
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(opcod) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, CampoReg)
                Select Case op2.Size 'carica l'immediato
                    Case ParamSize.Bits8
                        AddCodeByte(op2.Value)
                    Case ParamSize.Bits16
                        AddCodeWord(op2.Value)
                    Case ParamSize.Bits32
                        AddCodeDWord(op2.Value)
                End Select
            End If
        End If
    End Sub

    'r\m, r\m (8-16-32)
    'r\m, imm (8-16-32)
    Public Sub GruppoA1_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, OpCode2 As Byte, CampoReg As Byte)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size <> ParamSize.Bits8 Then OpCode2 = SetBit(OpCode2, 0) 'setta il bit w se non è una operazione a 8bit
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione
                AddCodeByte(EncodeModRM(&H3, CampoReg, RegToBin(op1.Value)))
                Select Case op2.Size 'carica l'immediato
                    Case ParamSize.Bits8
                        AddCodeByte(op2.Value)
                    Case ParamSize.Bits16
                        AddCodeWord(op2.Value)
                    Case ParamSize.Bits32
                        AddCodeDWord(op2.Value)
                End Select
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                OpCode1 = IIf(op2.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.BitsUnknown Then
                    If op2.Size <> ParamSize.Bits8 Then OpCode2 = SetBit(OpCode2, 0) 'setta il bit w se non è una operazione a 8bit
                Else
                    If op1.Size <> ParamSize.Bits8 Then OpCode2 = SetBit(OpCode2, 0) 'setta il bit w se non è una operazione a 8bit
                End If
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, CampoReg)
                Select Case op2.Size 'carica l'immediato
                    Case ParamSize.Bits8
                        AddCodeByte(op2.Value)
                    Case ParamSize.Bits16
                        AddCodeWord(op2.Value)
                    Case ParamSize.Bits32
                        AddCodeDWord(op2.Value)
                End Select
            End If
        End If
    End Sub

    'r\m\rel (16-32)
    Public Sub GruppoD_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, opcode2 As Byte, CampoReg As Byte)
        Dim op1 As ParsedOperand = op.Operands(0)
        '  Stop
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            AddCodeByte(opcode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(EncodeModRM(&H3, CampoReg, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            AddCodeByte(opcode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            WriteMem(op1.Value.ToString, CampoReg)

        ElseIf op1.Type = ParamType.ParamImm Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            Select Case op1.Size 'carica l'immediato
                Case ParamSize.Bits16
                    AddCodeWord(op1.Value)
                Case ParamSize.Bits32
                    AddCodeDWord(op1.Value)
            End Select
        End If
    End Sub

    'r\m (8-16-32) f6
    'r, r\m (16-32) 0f af
    'r, r\m, imm8 (16-32) 6b
    'r, r\m, imm (16-32) 69
    Public Sub GruppoG_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, OpCode2 As Byte, OpCode3 As Byte, CampoReg As Byte)
        Select Case op.Operands.Count
            Case 1
                Dim op1 As ParsedOperand = op.Operands(0)
                If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
                    If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                    OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                    AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                    AddCodeByte(EncodeModRM(&H3, CampoReg, RegToBin(op1.Value))) 'carica il mod reg rm

                ElseIf op1.Type = ParamType.ParamMem Then
                    If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                    If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                    OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                    AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                    WriteMem(op1.Value.ToString, CampoReg)
                End If

            Case 2
                Dim op1 As ParsedOperand = op.Operands(0)
                Dim op2 As ParsedOperand = op.Operands(1)
                If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
                    If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        AddCodeByte(&HF)
                        AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

                    ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                        AddCodeByte(&HF)
                        AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        WriteMem(op2.Value.ToString, RegToBin(op1.Value))
                    End If
                End If
            Case 3
                Dim op1 As ParsedOperand = op.Operands(0)
                Dim op2 As ParsedOperand = op.Operands(1)
                Dim op3 As ParsedOperand = op.Operands(2)
                If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
                    If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        OpCode3 = IIf(op3.Size = ParamSize.Bits8, SetBit(OpCode3, 1), OpCode3)
                        AddCodeByte(OpCode3) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

                    ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                        OpCode3 = IIf(op3.Size = ParamSize.Bits8, SetBit(OpCode3, 1), OpCode3)
                        AddCodeByte(OpCode3) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        WriteMem(op2.Value.ToString, RegToBin(op1.Value))
                    End If
                    Select Case op3.Size 'carica l'immediato
                        Case ParamSize.Bits8
                            AddCodeByte(op3.Value)
                        Case ParamSize.Bits16
                            AddCodeWord(op3.Value)
                        Case ParamSize.Bits32
                            AddCodeDWord(op3.Value)
                    End Select
                End If
        End Select
    End Sub

    'r\m (8-16-32) f6
    'r\m, r8 (8-16-32) 0f af
    'r\m, imm8 (16-32) 6b
    Public Sub GruppoH_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, OpCode2 As Byte, CampoReg As Byte)
        Select Case op.Operands.Count
            Case 1
                Dim op1 As ParsedOperand = op.Operands(0)
                If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
                    If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                    OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                    AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                    AddCodeByte(EncodeModRM(&H3, CampoReg, RegToBin(op1.Value))) 'carica il mod reg rm

                ElseIf op1.Type = ParamType.ParamMem Then
                    If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                    If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                    OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                    AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                    WriteMem(op1.Value.ToString, CampoReg)
                End If

            Case 2
                Dim op1 As ParsedOperand = op.Operands(0)
                Dim op2 As ParsedOperand = op.Operands(1)
                AddCodeByte(&HF)
                If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
                    If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                        OpCode1 = SetBit(OpCode1, 1)
                        AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm
                    ElseIf op2.Type = ParamType.ParamImm Then
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        OpCode2 = IIf(op1.Size = ParamSize.Bits8, OpCode2, SetBit(OpCode2, 0))
                        AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        AddCodeByte(EncodeModRM(&H3, CampoReg, RegToBin(op1.Value))) 'carica il mod reg rm
                        AddCodeByte(op2.Value)
                    End If
                ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                    If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                        OpCode1 = IIf(op1.Size = ParamSize.Bits8, OpCode1, SetBit(OpCode1, 0))
                        OpCode1 = SetBit(OpCode1, 1)
                        AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        WriteMem(op1.Value.ToString, RegToBin(op2.Value))
                    ElseIf op2.Type = ParamType.ParamImm Then
                        If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                        If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                        OpCode2 = IIf(op1.Size = ParamSize.Bits8, OpCode2, SetBit(OpCode2, 0))
                        AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                        WriteMem(op1.Value.ToString, CampoReg)
                    End If
                End If
        End Select
    End Sub

    'm (8-16-32) f6
    'r, r (8-16-32) 0f af
    Public Sub GruppoM_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, opcode2 As Byte, CampoReg As Byte)
        Select Case op.Operands.Count
            Case 1
                Dim op1 As ParsedOperand = op.Operands(0)
                If op1.Type = ParamType.ParamMem Then
                    OpCode1 = IIf(op1.Size = ParamSize.Bits32, OpCode1, SetBit(OpCode1, 2))
                    AddCodeByte(OpCode1)
                    WriteMem(op1.Value.ToString, CampoReg)
                End If

            Case 2
                Dim op1 As ParsedOperand = op.Operands(0)
                Dim op2 As ParsedOperand = op.Operands(1)
                If op1.Type = ParamType.ParamSTX Then 'se il primo operando è tipo registro
                    If op2.Type = ParamType.ParamSTX Then 'se il secondo operando è tipo registro
                        If op1.Value = "ST(0)" Then
                            AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                            AddCodeByte(Insert3Bits(opcode2, STXRegToBin(op2.Value), 0)) 'carica il mod reg rm
                        Else
                            OpCode1 = SetBit(OpCode1, 2)
                            AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                            AddCodeByte(Insert3Bits(opcode2, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm

                        End If
                    End If
                End If
        End Select
    End Sub

    'm (8-16-32) f6
    'r, r (8-16-32) 0f af
    Public Sub GruppoN_Istruzioni(ByVal op As ParsedInstruction, OpCode1 As Byte, opcode2 As Byte, CampoReg As Byte)
        Dim op1 As ParsedOperand = op.Operands(0)
        If op1.Type = ParamType.ParamMem Then
            OpCode1 = IIf(op1.Size = ParamSize.Bits32, OpCode1, SetBit(OpCode1, 2))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, CampoReg)

        ElseIf op1.Type = ParamType.ParamSTX Then 'se il primo operando è tipo registro
            AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(Insert3Bits(opcode2, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm
        End If
    End Sub

    Public Sub WriteMem(value1 As String, value2 As Byte)
        Dim s() As String, mem As String
        '    Stop
        mem = MemToBin(value1) 'codifica l'operando memoria
        If InStr(mem, " _ ") > 0 And InStr(mem, " - ") > 0 Then 'se è composta da mod 000 rm + displacement + sib
            s = Split(mem, " _ ") 'estrae la parte mod 000 rm
            AddCodeByte(ModifyMiddleBits(CByte(s(0)), value2)) 'carica mod reg rm, sostituendo nel campo reg il valore del primo operando registro
            s = Split(s(1), " - ") 'separa il displacement dal sib
            AddCodeByte(CByte(s(1))) 'carica il valore della codifica sib
            Select Case ImmediateType(CLng(s(0))) 'controlla la dimensione del displacement
                Case ParamSize.Bits8 'se 8bit
                    AddCodeByte(GetSecureImmediate(s(0)))
                Case ParamSize.Bits32 'se 32bit
                    AddCodeDWord(GetSecureImmediate(s(0)))
            End Select
        ElseIf InStr(mem, " _ ") > 0 And Not (InStr(mem, " - ") > 0) Then 'se è composta da mod 000 rm + displacement 
            s = Split(mem, " _ ") 'estrae la parte mod 000 rm
            AddCodeByte(ModifyMiddleBits(CByte(s(0)), value2)) 'carica mod reg rm, sostituendo nel campo reg il valore del primo operando registro
            Select Case ImmediateType(CLng(s(1))) 'controlla la dimensione del displacement
                Case ParamSize.Bits8 'se 8bit
                    AddCodeByte(GetSecureImmediate(s(1)))
                Case ParamSize.Bits32  'se 32bit
                    AddCodeDWord(GetSecureImmediate(s(1)))
            End Select
        ElseIf Not (InStr(mem, " _ ") > 0) And InStr(mem, " - ") > 0 Then 'se è composta da mod 000 rm + sib 
            s = Split(mem, " - ") 'estrae la parte mod 000 rm
            AddCodeByte(ModifyMiddleBits(CByte(s(0)), value2)) 'carica mod reg rm, sostituendo nel campo reg il valore del primo operando registro
            AddCodeByte(CByte(s(1))) 'carica il valore della codifica sib
        ElseIf Not (InStr(mem, " _ ") > 0 And InStr(mem, " - ") > 0) Then 'se è composta da mod 000 rm 
            AddCodeByte(ModifyMiddleBits(CByte(mem), value2)) 'carica mod reg rm, sostituendo nel campo reg il valore del primo operando registro
        End If
    End Sub
End Module
