
Module Cod_istruzioni
    'd       source        destination
    '0       reg           memory
    '1       memory        reg
    '0       reg           rm
    '1       rm            reg
    Public Sub datatobin(ByVal op As ParsedInstruction)
        For i As Integer = 0 To op.Operands.Count - 1
            Dim operand As ParsedOperand = op.Operands(i)
            If operand.Type = ParamType.ParamImm Then
                Select Case operand.Size
                    Case ParamSize.Bits8
                        AddDataByte(operand.Value)
                    Case ParamSize.Bits16
                        AddDataWord(operand.Value)
                    Case ParamSize.Bits32
                        AddDataDWord(operand.Value)
                End Select
            End If
        Next
    End Sub

    Public Sub resdatatobin(ByVal op As ParsedInstruction)
        Dim operand As ParsedOperand = op.Operands(0)
        If operand.Type = ParamType.ParamImm Then
            For i As Integer = 0 To operand.Value
                AddDataByte(operand.Value)
            Next
        End If
    End Sub

    Public Sub aaatobin()
        AddCodeByte(&H37)
    End Sub

    Public Sub aadtobin()
        AddCodeByte(&HD5)
        AddCodeByte(&HA0)
    End Sub

    Public Sub aamtobin()
        AddCodeByte(&HD4)
        AddCodeByte(&HA0)
    End Sub

    Public Sub aastobin()
        AddCodeByte(&H3F)
    End Sub

    'adc operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 000100dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 010 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0001010w  datalow datahight (if w=1 ax)
    Public Sub adctobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H10, &H14, &H2)
    End Sub

    'add operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 000000dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 000 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0000010w  datalow datahight (if w=1 ax)
    Public Sub addtobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H0, &H4, &H0)
    End Sub

    'and operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 001000dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 100 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0010010w  datalow datahight (if w=1 ax)
    Public Sub andtobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H20, &H24, &H4)
    End Sub

    'bound operando1, operando2
    'reg,[mem] 01100010 mod reg r/m scale index base displow disphigh 
    Public Sub boundtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&H62) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If
    End Sub

    'bsf operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 0000111110111100 mod reg r/m scale index base displow disphigh 
    Public Sub bsftobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBC) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBC) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If
    End Sub

    'bsr operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 0000111110111101 mod reg r/m scale index base displow disphigh 
    Public Sub bsrtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBD) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBD) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If

    End Sub

    'bswap operando
    'reg 0000111111001 reg 
    Public Sub bswaptobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        AddCodeByte(&HF) 'carica l'opcode dell'istruzione 
        AddCodeByte(&HC8) 'carica l'opcode dell'istruzione
        AddCodeByte(Insert3Bits(&HC8, RegToBin(op1.Value), 0)) 'carica il mod reg rm
    End Sub

    'bt operando1, operando2
    '[reg/mem],reg o reg,reg 0000111110100011 mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 00001111 10111010 mod 100 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    Public Sub bttobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HA3) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm


            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                AddCodeByte(EncodeModRM(&H3, &H4, RegToBin(op1.Value)))
                AddCodeByte(op2.Value) 'carica immediato
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HA3) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, &H4)
                AddCodeByte(op2.Value) 'carica immediato
            End If
        End If
    End Sub

    'btc operando1, operando2
    '[reg/mem],reg o reg,reg 0000111110111011 mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 00001111 10111010 mod 111 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    Public Sub btctobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBB) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm


            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                AddCodeByte(EncodeModRM(&H3, &H7, RegToBin(op1.Value)))
                AddCodeByte(op2.Value) 'carica immediato
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBB) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, &H7)
                AddCodeByte(op2.Value) 'carica immediato
            End If
        End If
    End Sub

    'btr operando1, operando2
    '[reg/mem],reg o reg,reg 0000111110110011 mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 00001111 10111010 mod 110 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    Public Sub btrtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HB3) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm


            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                AddCodeByte(EncodeModRM(&H3, &H6, RegToBin(op1.Value)))
                AddCodeByte(op2.Value) 'carica immediato
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HB3) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, &H6)
                AddCodeByte(op2.Value) 'carica immediato
            End If
        End If
    End Sub

    'bts operando1, operando2
    '[reg/mem],reg o reg,reg 0000111110101011 mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 00001111 10111010 mod 101 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    Public Sub btstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HAB) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm


            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                AddCodeByte(EncodeModRM(&H3, &H5, RegToBin(op1.Value)))
                AddCodeByte(op2.Value) 'carica immediato
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HAB) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HBA) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, &H5)
                AddCodeByte(op2.Value) 'carica immediato
            End If
        End If
    End Sub

    'call operando 
    'direct within segment near i16 o i32 11101000 displow disphigh
    'indirect within segment near mem16 o reg16 o mem32 o reg32 11111111 mod 010 rm displow disphigh
    Public Sub CALLtobin(ByVal op As ParsedInstruction)
        GruppoD_Istruzioni(op, &HE8, &HFF, &H2)
    End Sub

    Public Sub cbwtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H98)
    End Sub

    Public Sub cwdetobin()
        AddCodeByte(&H98)
    End Sub

    Public Sub cwdtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H99)
    End Sub

    Public Sub cdqtobin()
        AddCodeByte(&H99)
    End Sub

    Public Sub clctobin()
        AddCodeByte(&HF8)
    End Sub

    Public Sub cldtobin()
        AddCodeByte(&HFC)
    End Sub

    Public Sub clitobin()
        AddCodeByte(&HFA)
    End Sub

    Public Sub cmctobin()
        AddCodeByte(&HF5)
    End Sub

    'cmp operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 001110dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 111 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0011110w  datalow datahight (if w=1 ax)
    Public Sub cmptobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H38, &H3C, &H7)
    End Sub

    'cmov cc operando1, operando2
    'cc reg,[reg/mem]/reg o reg,reg 000011110100 cc mod reg r/m scale index base displow disphigh
    Public Sub cmovtobin(ByVal op As ParsedInstruction, Condiction As Byte)
        Dim s() As String, mem As String
        If op.Operands.Count <> 2 Then
            Console.WriteLine("Numero di operandi errato per L'istruzione")
        End If
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size <> op2.Size Then Console.WriteLine("L'istruzione per questa configurazione, ammette solo operandi della stessa dimensione") 'se i due operandi hanno dimensioni diverse
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(Insert4Bits(&H40, Condiction, 0))
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size <> op2.Size And op2.Size <> ParamSize.BitsUnknown Then Console.WriteLine("L'istruzione per questa configurazione, ammette solo operandi della stessa dimensione") 'se i due operandi hanno dimensioni diverse e l'operando memoria ha una dimensione specifica
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(Insert4Bits(&H40, Condiction, 0))
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If
    End Sub

    Public Sub cmpsbtobin()
        AddCodeByte(&HA6)
    End Sub

    Public Sub cmpswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&HA7)
    End Sub

    Public Sub cmpsdtobin()
        AddCodeByte(&HA7)
    End Sub

    Public Sub insbtobin()
        AddCodeByte(&H6C)
    End Sub

    Public Sub inswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H6D)
    End Sub

    Public Sub insdtobin()
        AddCodeByte(&H6D)
    End Sub

    Public Sub outsbtobin()
        AddCodeByte(&H6E)
    End Sub

    Public Sub outswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H6F)
    End Sub

    Public Sub outsdtobin()
        AddCodeByte(&H6F)
    End Sub

    'cmpxchg operando1, operando2
    '[reg/mem],reg o reg,reg 00001111 1011000w mod reg r/m scale index base displow disphigh 
    Public Sub cmpxchgtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HB0, SetBit(&HB0, 0))
                AddCodeByte(&HF)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand overriden
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address overriden
                Dim OpCode1 As Byte = IIf(op2.Size = ParamSize.Bits8, &HB0, SetBit(&HB0, 0))
                AddCodeByte(&HF)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit 
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
            End If
        End If
    End Sub

    Public Sub daatobin()
        AddCodeByte(&H27)
    End Sub

    Public Sub dastobin()
        AddCodeByte(&H2F)
    End Sub

    'dec operando1, operando2
    'reg/[reg/mem] 1111111w mod 001 r/m scale index base displow disphigh 
    Public Sub dectobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HFE, SetBit(&HFE, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H1, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HFE, SetBit(&HFE, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H1)
        End If
    End Sub

    'div operando1, operando2
    'reg/[reg/mem] 1111011w mod 110 r/m scale index base displow disphigh 
    Public Sub divtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H6, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H6)
        End If
    End Sub

    Public Sub hlttobin()
        AddCodeByte(&HF4)
    End Sub

    'idiv operando1, operando2
    'reg/[reg/mem] 1111011w mod 111 r/m scale index base displow disphigh 
    Public Sub idivtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H7, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H7)
        End If
    End Sub

    'imul operando1, operando2
    '[reg/mem]/reg 1111011w mod 101 r/m scale index base displow disphigh 
    'reg,[reg/mem] o reg,reg 00001111 10101111 mod reg r/m scale index base displow disphigh 
    'reg,reg/[reg/mem],imm 011010s1 mod reg r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    Public Sub imultobin(ByVal op As ParsedInstruction)
        GruppoG_Istruzioni(op, &HF6, &HAF, &H69, &H5)
    End Sub

    'inc operando1, operando2
    'reg/[reg/mem] 1111111w mod 000 r/m scale index base displow disphigh 
    Public Sub inctobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HFE, SetBit(&HFE, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H0, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HFE, SetBit(&HFE, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H0)
        End If
    End Sub

    Public Sub intobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(IIf(op1.Size = ParamSize.Bits8, &HEC, &HED)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit

            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(IIf(op1.Size = ParamSize.Bits8, &HE4, &HE5)) 'carica l'opcode dell'istruzione
                AddCodeByte(op2.Value) 'carica immediato
            End If
        End If
    End Sub

    'int operando1, operando2
    'i8 1110011w imm  
    Public Sub outtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(IIf(op1.Size = ParamSize.Bits8, &HEE, &HEF)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            End If
        ElseIf op1.Type = ParamType.ParamImm Then
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(IIf(op2.Size = ParamSize.Bits8, &HE6, &HE7)) 'carica l'opcode dell'istruzione
                AddCodeByte(op1.Value)
            End If
        End If
    End Sub

    'j cc operando o operando1
    '0111 cc structure displow disphigh 
    Public Sub jtobin(ByVal op As ParsedInstruction, condiction As Byte)
        Dim op1 As ParsedOperand = op.Operands(0)
        Select Case op1.Size 'carica l'immediato
            Case ParamSize.Bits8
                AddCodeByte(Insert4Bits(&H70, condiction, 0))
                AddCodeByte(op1.Value)
            Case ParamSize.Bits16
                AddCodeByte(&H66) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(&HF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(Insert4Bits(&H80, condiction, 0))
                AddCodeWord(op1.Value)
            Case ParamSize.Bits32
                AddCodeByte(&HF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(Insert4Bits(&H80, condiction, 0))
                AddCodeDWord(op1.Value)
        End Select
    End Sub

    'jcxz operando
    'i8 11100011 immediate
    Public Sub jcxztobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        AddCodeByte(&HE3)
        AddCodeByte(op1.Value)
    End Sub

    'lia operando
    'i8 11100011 immediate
    Public Sub liatobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        AddCodeByte(&HF)
        AddCodeByte(&H0)
        AddCodeDWord(op1.Value)
    End Sub

    'jecxz operando
    'i8 11100011 immediate
    Public Sub jecxztobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        AddCodeByte(&H66)
        AddCodeByte(&HE3)
        AddCodeByte(op1.Value)
    End Sub

    'jmp operando 
    'direct near i8 11101011 disp8bit
    'direct imm 11101001 displow disphigh
    'indirect mem o reg 11111111 mod 100 rm displow disphigh
    Public Sub JMPtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            AddCodeByte(&HFF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(EncodeModRM(&H3, &H4, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            AddCodeByte(&HFF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            WriteMem(op1.Value.ToString, &H4)

        ElseIf op1.Type = ParamType.ParamImm Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, SetBit(&HE9, 1), &HE9)
            AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            Select Case op1.Size 'carica l'immediato
                Case ParamSize.Bits8
                    AddCodeByte(op1.Value)
                Case ParamSize.Bits16
                    AddCodeWord(op1.Value)
                Case ParamSize.Bits32
                    AddCodeDWord(op1.Value)
            End Select
        End If

    End Sub

    Public Sub lahftobin()
        AddCodeByte(&H9F)
    End Sub

    'lea operando1, operando2
    'reg,[reg/mem] 10001101 mod reg r/m scale index base displow disphigh 
    Public Sub leatobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&H8D) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If
    End Sub

    Public Sub lodsbtobin()
        AddCodeByte(&HAC)
    End Sub

    Public Sub lodswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&HAD)
    End Sub

    Public Sub lodsdtobin()
        AddCodeByte(&HAD)
    End Sub

    'loop operando
    'i8 11100010 immediate
    Public Sub looptobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamImm And op1.Size = ParamSize.Bits8 Then
            AddCodeByte(&HE2) 'carica l'opcode dell'istruzione
            AddCodeByte(op1.Value)
        End If
    End Sub

    'loopez operando
    'i8 11100001 immediate
    Public Sub loopeztobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamImm And op1.Size = ParamSize.Bits8 Then
            AddCodeByte(&HE1) 'carica l'opcode dell'istruzione
            AddCodeByte(op1.Value)
        End If
    End Sub

    'loopnez operando
    'i8 11100000 immediate
    Public Sub loopneztobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamImm And op1.Size = ParamSize.Bits8 Then
            AddCodeByte(&HE0) 'carica l'opcode dell'istruzione
            AddCodeByte(op1.Value)
        End If
    End Sub

    'mov operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 100010dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 1100011w mod 000 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator,[imm] [imm],accumulator 101000dw  datalow datahight (if w=1 ax)
    'reg/[reg/mem],sreg sreg,reg/[reg/mem] 100011d0 mod reg r/m scale index base displow disphigh 
    Public Sub movtobin(ByVal op As ParsedInstruction)
        GruppoA1_Istruzioni(op, &H88, &HC6, &H0)
    End Sub

    Public Sub movsbtobin()
        AddCodeByte(&HA4)
    End Sub

    Public Sub movswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&HA5)
    End Sub

    Public Sub movsdtobin()
        AddCodeByte(&HA5)
    End Sub

    'movsx operando1, operando2
    'reg,[reg/mem]  o reg,reg 000011111011111w mod reg r/m scale index base displow disphigh 
    Public Sub movsxtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(IIf(op2.Size = ParamSize.Bits8, &HBE, &HBF)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size <> op2.Size And op2.Size <> ParamSize.BitsUnknown Then Console.WriteLine("L'istruzione per questa configurazione, ammette solo operandi della stessa dimensione") 'se i due operandi hanno dimensioni diverse e l'operando memoria ha una dimensione specifica
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(IIf(op2.Size = ParamSize.Bits8, &HBE, &HBF)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If
    End Sub

    'movzx operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 000011111011011w mod reg r/m scale index base displow disphigh 
    Public Sub movzxtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(IIf(op2.Size = ParamSize.Bits8, &HB6, &HB7)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size <> op2.Size And op2.Size <> ParamSize.BitsUnknown Then Console.WriteLine("L'istruzione per questa configurazione, ammette solo operandi della stessa dimensione") 'se i due operandi hanno dimensioni diverse e l'operando memoria ha una dimensione specifica
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(IIf(op2.Size = ParamSize.Bits8, &HB6, &HB7)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If
        End If
    End Sub

    'mul operando1, operando2
    'reg/[reg/mem] 1111011w mod 100 r/m scale index base displow disphigh 
    Public Sub multobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H4, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H4)
        End If
    End Sub

    'neg operando1
    'reg/[reg/mem] 1111011w mod 011 r/m scale index base displow disphigh 
    Public Sub negtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H3, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H3)
        End If
    End Sub

    Public Sub noptobin()
        AddCodeByte(&H90)
    End Sub

    'not operando1
    'reg/[reg/mem] 1111011w mod 010 r/m scale index base displow disphigh 
    Public Sub nottobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            AddCodeByte(EncodeModRM(&H3, &H2, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HF6, SetBit(&HF6, 0))
            AddCodeByte(OpCode1)
            WriteMem(op1.Value.ToString, &H2)
        End If
    End Sub

    'or operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 000010dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 001 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0000110w  datalow datahight (if w=1 ax)
    Public Sub ortobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H8, &HC, &H1)
    End Sub

    'pop operando1
    'reg/[reg/mem] 10001111 mod 000 r/m scale index base displow disphigh 
    'ds,es,ss 000 sreg2 111
    'fs,gs 00001111 10 sreg3 001
    Public Sub poptobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            AddCodeByte(&H8F)
            AddCodeByte(EncodeModRM(&H3, &H0, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            AddCodeByte(&H8F)
            WriteMem(op1.Value.ToString, &H0)
        End If
    End Sub

    Public Sub popatobin()
        AddCodeByte(&H61)
    End Sub

    Public Sub popadtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H61)
    End Sub

    Public Sub popftobin()
        AddCodeByte(&H9D)
    End Sub

    Public Sub popfdtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H9D)
    End Sub

    'Push(operando1
    'reg/[reg/mem] 11111111 mod 110 r/m scale index base displow disphigh 
    'imm 011010s0 immediate
    Public Sub pushtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            AddCodeByte(&HFF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(EncodeModRM(&H3, &H6, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            AddCodeByte(&HFF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            WriteMem(op1.Value.ToString, &H6)

        ElseIf op1.Type = ParamType.ParamImm Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, SetBit(&HA6, 1), &HA6)
            AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            Select Case op1.Size 'carica l'immediato
                Case ParamSize.Bits8
                    AddCodeByte(op1.Value)
                Case ParamSize.Bits16
                    AddCodeWord(op1.Value)
                Case ParamSize.Bits32
                    AddCodeDWord(op1.Value)
            End Select
        End If

    End Sub

    Public Sub pushatobin()
        AddCodeByte(&H60)
    End Sub

    Public Sub pushadtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H60)
    End Sub

    Public Sub pushftobin()
        AddCodeByte(&H9C)
    End Sub

    Public Sub pushfdtobin()
        AddCodeByte(&H66)
        AddCodeByte(&H9C)
    End Sub

    'rcl operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 010 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 010 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 010 r/m scale index base displow disphigh
    Public Sub rcltobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H2)
    End Sub

    'rcr operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 011 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 011 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 011 r/m scale index base displow disphigh
    Public Sub rcrtobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H3)
    End Sub

    'ret operando 
    'no operand  11000011 
    'i16 11000010 displow disphigh
    Public Sub rettobin(ByVal op As ParsedInstruction)
        If op.Operands.Count = 0 Then
            AddCodeByte(&HC3)
        Else
            Dim op1 As ParsedOperand = op.Operands(0)
            If op1.Type = ParamType.ParamImm And op1.Size = ParamSize.Bits16 Then
                AddCodeByte(&HC2) 'carica l'opcode dell'istruzione
                AddCodeWord(op1.Value)
            End If
        End If
    End Sub

    Public Sub irettobin()
        AddCodeByte(&HCF)
    End Sub

    'rol operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 000 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 000 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 000 r/m scale index base displow disphigh
    Public Sub roltobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H0)
    End Sub

    'ror operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 001 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 001 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 001 r/m scale index base displow disphigh
    Public Sub rortobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H1)
    End Sub

    Public Sub sahftobin()
        AddCodeByte(&H9E)
    End Sub

    'shl o sal operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 100 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 100 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 100 r/m scale index base displow disphigh
    Public Sub shltobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H4)
    End Sub

    'sar o sal operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 111 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 111 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 111 r/m scale index base displow disphigh
    Public Sub sartobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H7)
    End Sub

    'sbb operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 000110dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 011 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0001110w  datalow datahight (if w=1 ax)
    Public Sub sbbtobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H18, &H1C, &H3)
    End Sub

    Public Sub scasbtobin()
        AddCodeByte(&HAE)
    End Sub

    Public Sub scaswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&HAF)
    End Sub

    Public Sub scasdtobin()
        AddCodeByte(&HAF)
    End Sub

    'set cc operando o operando1
    'mem8/reg8 000011111001 cc structure mod 000 r/m scale index base displow disphigh 
    Public Sub settobin(ByVal op As ParsedInstruction, condiction As Byte)
        Dim op1 As ParsedOperand = op.Operands(0)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            AddCodeByte(&HF)
            AddCodeByte(Insert4Bits(&H90, condiction, 0)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(EncodeModRM(&H3, &H0, RegToBin(op1.Value))) 'carica il mod reg rm

        ElseIf op1.Type = ParamType.ParamMem Then
            If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
            If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            AddCodeByte(&HF)
            AddCodeByte(Insert4Bits(&H90, condiction, 0)) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            WriteMem(op1.Value.ToString, &H0)
        End If
    End Sub

    'shld o sal operando1, operando2
    '[reg/mem]/reg,reg,cl  0000111110100101 mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],reg,imm8 0000111110100100 mod reg r/m scale index base displow disphigh disp
    Public Sub shldtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op3.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HA5) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm
            ElseIf op3.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HA4) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm
                AddCodeByte(op3.Value)
            End If
        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op3.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HA5) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
            ElseIf op3.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HA4) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
                AddCodeByte(op3.Value)
            End If
        End If
    End Sub

    'shr operando1, operando2
    '[reg/mem],cl o reg,cl 1101001w mod 101 r/m scale index base displow disphigh 
    'reg/[reg/mem],imm8 1100000w mod 101 r/m  disp
    'reg/[reg/mem],1 1101000w  mod 101 r/m scale index base displow disphigh
    Public Sub shrtobin(ByVal op As ParsedInstruction)
        GruppoH_Istruzioni(op, &HD0, &HC0, &H5)
    End Sub

    'shrd o sal operando1, operando2
    '[reg/mem]/reg,reg,cl  0000111110101101 mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],reg,imm8 0000111110101100 mod reg r/m scale index base displow disphigh disp
    Public Sub shrdtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op3.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HAD) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm
            ElseIf op3.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                AddCodeByte(&HF)
                AddCodeByte(&HAC) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm
                AddCodeByte(op3.Value)
            End If
        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If op3.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HAD) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
            ElseIf op3.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HAC) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
                AddCodeByte(op3.Value)
            End If
        End If
    End Sub

    Public Sub stctobin()
        AddCodeByte(&HF9)
    End Sub

    Public Sub stdtobin()
        AddCodeByte(&HFD)
    End Sub

    Public Sub stitobin()
        AddCodeByte(&HFB)
    End Sub
    Public Sub stosbtobin()
        AddCodeByte(&HAA)
    End Sub

    Public Sub stoswtobin()
        AddCodeByte(&H66)
        AddCodeByte(&HAB)
    End Sub

    Public Sub stosdtobin()
        AddCodeByte(&HAB)
    End Sub

    'sub operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 001010dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 101 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0010110w  datalow datahight (if w=1 ax)
    Public Sub subtobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H28, &H2C, &H5)
    End Sub

    'test operando1, operando2
    '[reg/mem],reg o reg,reg 1000010w mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 1111011w mod 000 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 1010100w  datalow datahight (if w=1 ax)
    Public Sub testtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &H84, SetBit(&H84, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamImm Then
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If (op1.Value = ASMRegisters.RegAL Or op1.Value = ASMRegisters.RegAX Or op1.Value = ASMRegisters.RegEAX) And op1.Size <> op2.Size Then 'controlla se si tratta di istruzione rapida con accumulatore 
                    Dim OpCode2 As Byte = IIf(op1.Size = ParamSize.Bits8, &HA8, SetBit(&HA8, 0))
                    AddCodeByte(OpCode2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                    If op1.Size = ParamSize.Bits8 Then 'controlla la dimensione dell'immediato e lo carica
                        AddCodeByte(op2.Value)
                    ElseIf op1.Size = ParamSize.Bits16 Then
                        AddCodeWord(op2.Value)
                    Else
                        AddCodeDWord(op2.Value)
                    End If
                Else 'se non è una operazione rapida con acumulatore
                    Dim opcod As Byte = &HF6
                    If op1.Size <> ParamSize.Bits8 Then opcod = SetBit(opcod, 0) 'setta il bit w se non è una operazione a 8bit
                    If op1.Size <> op2.Size And op2.Size = ParamSize.Bits8 Then opcod = SetBit(opcod, 1) 'setta il bit s se non è una operazione sign extend
                    AddCodeByte(opcod) 'carica l'opcode dell'istruzione
                    AddCodeByte(EncodeModRM(&H3, &H0, RegToBin(op1.Value)))
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
                Dim OpCode1 As Byte = IIf(op2.Size = ParamSize.Bits8, &H84, SetBit(&H84, 0))
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))

            ElseIf op2.Type = ParamType.ParamImm Then
                Dim opcod As Byte = &HF6
                If op1.Size = ParamSize.BitsUnknown Then
                    If op2.Size <> ParamSize.Bits8 Then opcod = SetBit(opcod, 0) 'setta il bit w se non è una operazione a 8bit
                Else
                    If op1.Size <> ParamSize.Bits8 Then opcod = SetBit(opcod, 0) 'setta il bit w se non è una operazione a 8bit
                    If op1.Size <> op2.Size And op2.Size = ParamSize.Bits8 Then opcod = SetBit(opcod, 1) 'setta il bit s se non è una operazione sign extend
                End If
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(opcod) 'carica l'opcode dell'istruzione
                WriteMem(op1.Value.ToString, &H0)
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

    'xadd operando1, operando2
    '[reg/mem],reg o reg,reg 000011111100000w mod reg r/m scale index base displow disphigh 
    Public Sub xaddtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &HC0, SetBit(&HC0, 0))
                AddCodeByte(&HF)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op2.Value), RegToBin(op1.Value))) 'carica il mod reg rm
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand overriden
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address overriden
                Dim OpCode1 As Byte = IIf(op2.Size = ParamSize.Bits8, &HC0, SetBit(&HC0, 0))
                AddCodeByte(&HF)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit 
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
            End If
        End If
    End Sub

    'xchg operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 1000011w mod reg r/m scale index base displow disphigh 
    Public Sub xchgtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &H86, SetBit(&H86, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If op1.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                Dim OpCode1 As Byte = IIf(op1.Size = ParamSize.Bits8, &H86, SetBit(&H86, 0))
                OpCode1 = SetBit(OpCode1, 1)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, RegToBin(op1.Value))
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamReg Then
                If op2.Size = ParamSize.Bits16 Then AddCodeByte(&H66) 'se si trata di operandi a 16 bit setta il prefisso operand override
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                Dim OpCode1 As Byte = IIf(op2.Size = ParamSize.Bits8, &H86, SetBit(&H86, 0))
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, RegToBin(op2.Value))
            End If
        End If
    End Sub

    Public Sub xlatbtobin()
        AddCodeByte(&HD7)
    End Sub

    'xor operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 001100dw mod reg r/m scale index base displow disphigh 
    'reg/[reg/mem],imm 100000sw mod 110 r/m  displow disphigh datalow datahigh (data 16bit if s:w=01) s=0 no sign extension, s=1 if w=1 8bit imm sign extension to 16bit
    'accumulator, imm 0011010w  datalow datahight (if w=1 ax)
    Public Sub xortobin(ByVal op As ParsedInstruction)
        GruppoA_Istruzioni(op, &H30, &H34, &H6)
    End Sub

    Public Sub repetobin()
        AddCodeByte(&HF3)
    End Sub

    Public Sub repnetobin()
        AddCodeByte(&HF2)
    End Sub

    'fadd operando1, operando2
    '[reg/mem] o st(),st() 11011d00 mod 000 r/m scale index base displow disphigh 
    Public Sub faddtobin(ByVal op As ParsedInstruction)
        GruppoM_Istruzioni(op, &HD8, &HC0, &H0)
    End Sub


    'fcom operando1, operando2
    '[reg/mem] o st(),st() 11011d00 mod 010 r/m scale index base displow disphigh 
    Public Sub fcomtobin(ByVal op As ParsedInstruction)
        GruppoN_Istruzioni(op, &HD8, &HD0, &H2)
    End Sub

    'fcomp operando1, operando2
    '[reg/mem] o st(),st() 11011d00 mod 011 r/m scale index base displow disphigh 
    Public Sub fcomptobin(ByVal op As ParsedInstruction)
        GruppoN_Istruzioni(op, &HD8, &HD8, &H3)
    End Sub

    'fcomi operando1, operando2
    'st(),st() 11011111 mod 110 r/m scale index base displow disphigh 
    Public Sub fcomitobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        AddCodeByte(&HDB) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
        AddCodeByte(Insert3Bits(&HF0, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm
    End Sub

    'fcomip operando1, operando2
    'st(),st() 11011111 mod 110 r/m scale index base displow disphigh 
    Public Sub fcomiptobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        AddCodeByte(&HDF) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
        AddCodeByte(Insert3Bits(&HF0, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm
    End Sub

    'fdiv operando1, operando2
    '[reg/mem] o st(),st() 11011d00 mod 110 r/m scale index base displow disphigh 
    Public Sub fdivtobin(ByVal op As ParsedInstruction)
        GruppoM_Istruzioni(op, &HD8, &HF0, &H6)
    End Sub

    'fld operando1
    '[reg/mem] o st(i) 11011d01 mod 000 o 101 se mem80 r/m scale index base displow disphigh 
    Public Sub fldtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        If op1.Type = ParamType.ParamMem Then
            Dim opcode1 As Byte = &HD9
            opcode1 = IIf(op1.Size = ParamSize.Bits32, opcode1, SetBit(opcode1, 2))
            opcode1 = IIf(op1.Size = ParamSize.Bits80, SetBit(opcode1, 1), opcode1)
            Dim camporeg As Byte = IIf(op1.Size = ParamSize.Bits80, &H5, &H0)
            WriteMem(op1.Value.ToString, camporeg)

        ElseIf op1.Type = ParamType.ParamSTX Then 'se il primo operando è tipo registro
            AddCodeByte(&HD9) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(Insert3Bits(&HC0, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm
        End If
    End Sub

    'fmul operando1, operando2
    '[reg/mem] o st(),st() 11011d00 mod 001 r/m scale index base displow disphigh 
    Public Sub fmultobin(ByVal op As ParsedInstruction)
        GruppoM_Istruzioni(op, &HD8, &HC8, &H1)
    End Sub

    'fsqrt
    ' 1101100111111010
    Public Sub fsqrttobin()
        AddCodeByte(&HD9)
        AddCodeByte(&HFA)
    End Sub

    Public Sub fscaletobin()
        AddCodeByte(&HD9)
        AddCodeByte(&HFD)
    End Sub

    'fst operando1, operando2
    '[reg/mem] o st(),st() 11011d01 mod 010 r/m scale index base displow disphigh 
    Public Sub fsttobin(ByVal op As ParsedInstruction)
        GruppoN_Istruzioni(op, &HD9, &HD0, &H2)
    End Sub

    'fstp operando1
    '[reg/mem] o st(i) 11011d01 mod 011 o 111 se mem80 r/m scale index base displow disphigh 
    Public Sub fstptobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        If op1.Type = ParamType.ParamMem Then
            Dim opcode1 As Byte = &HD9
            opcode1 = IIf(op1.Size = ParamSize.Bits32, opcode1, SetBit(opcode1, 2))
            opcode1 = IIf(op1.Size = ParamSize.Bits80, SetBit(opcode1, 1), opcode1)
            Dim camporeg As Byte = IIf(op1.Size = ParamSize.Bits80, &H7, &H3)
            WriteMem(op1.Value.ToString, camporeg)

        ElseIf op1.Type = ParamType.ParamSTX Then 'se il primo operando è tipo registro
            AddCodeByte(&HD9) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(Insert3Bits(&HD8, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm
        End If
    End Sub

    'fsub operando1, operando2
    '[reg/mem] o st(),st() 11011d00 mod 100 r/m scale index base displow disphigh 
    Public Sub fsubtobin(ByVal op As ParsedInstruction)
        GruppoM_Istruzioni(op, &HD8, &HE0, &H4)
    End Sub

    Public Sub fxchtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        If op1.Type = ParamType.ParamSTX Then 'se il primo operando è tipo registro
            AddCodeByte(&HD9) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(Insert3Bits(&HC8, STXRegToBin(op1.Value), 0)) 'carica il mod reg rm
        End If
    End Sub

    'addpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011000 mod reg r/m scale index base displow disphigh 
    Public Sub addpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H58) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H58) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If

    End Sub

    'andpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101010100 mod reg r/m scale index base displow disphigh 
    Public Sub andpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H54) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H54) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If

    End Sub

    'cmppd operando1, operando2
    'reg,[reg/mem]/reg,imm8 o reg,reg 01100110 00001111 11000010 mod reg r/m scale index base displow disphigh imm
    Public Sub cmppdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&HC2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HC2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
            AddCodeByte(op3.Value) 'carica l'immediato
        End If

    End Sub

    'cvtsi2sd operando1, operando2
    'reg,[reg/mem] o reg,reg 111100100000111100101010 mod reg r/m scale index base displow disphigh 
    Public Sub cvtsi2sdtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF2)
                AddCodeByte(&HF)
                AddCodeByte(&H2A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF2)
                AddCodeByte(&HF)
                AddCodeByte(&H2A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'cvtpd2ps operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011010 mod reg r/m scale index base displow disphigh 
    Public Sub cvtpd2pstobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If

    End Sub

    'cvtps2pd operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011010 mod reg r/m scale index base displow disphigh 
    Public Sub cvtps2pdtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'divpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011110 mod reg r/m scale index base displow disphigh 
    Public Sub divpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5E) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5E) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'maxpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011111 mod reg r/m scale index base displow disphigh 
    Public Sub maxpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5F) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5F) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'minpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011101 mod reg r/m scale index base displow disphigh 
    Public Sub minpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5D) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5D) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'movapd operando1, operando2
    'reg,[reg/mem] o reg,reg 01100110000011110010100d mod reg r/m scale index base displow disphigh 
    Public Sub movapdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H28) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H28) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'movupd operando1, operando2
    'reg,[reg/mem] o reg,reg 01100110000011110001000d mod reg r/m scale index base displow disphigh 
    Public Sub movupdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H10) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67)
                AddCodeByte(&HF)
                AddCodeByte(&H10) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamMM Then
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67)
                Dim OpCode1 As Byte = SetBit(&H10, 0)
                AddCodeByte(&HF)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, MMRegToBin(op2.Value))
            End If
        End If
    End Sub

    'mulpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011001 mod reg r/m scale index base displow disphigh 
    Public Sub mulpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H59) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H59) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'orpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101010110 mod reg r/m scale index base displow disphigh 
    Public Sub orpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H56) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H56) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'shufpd operando1, operando2
    'reg,[reg/mem]/reg,imm8 o reg,reg 01100110 00001111 11000110 mod reg r/m scale index base displow disphigh imm
    Public Sub shufpdtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        AddCodeByte(&H66)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&HC6) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HC6) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
            AddCodeByte(op3.Value) 'carica l'immediato
        End If
    End Sub

    'subpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101011100 mod reg r/m scale index base displow disphigh 
    Public Sub subpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5C) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5C) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'unpckhpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111100010101 mod reg r/m scale index base displow disphigh 
    Public Sub unpckhpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H15) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H15) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'unpcklpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111100010100 mod reg r/m scale index base displow disphigh 
    Public Sub unpcklpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H14) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H14) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'xorpd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111101010111 mod reg r/m scale index base displow disphigh 
    Public Sub xorpdtobin(ByVal op As ParsedInstruction)
        AddCodeByte(&H66)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H57) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H57) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'paddd operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111111111110 mod reg r/m scale index base displow disphigh 
    Public Sub padddtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Size = ParamSize.Bits128 Then AddCodeByte(&H66)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro 
                AddCodeByte(&HF)
                AddCodeByte(&HFE) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HFE) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'pmullw operando1, operando2
    'reg,[reg/mem] o reg,reg 011001100000111111010101 mod reg r/m scale index base displow disphigh 
    Public Sub pmullwtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Size = ParamSize.Bits128 Then AddCodeByte(&H66)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro 
                AddCodeByte(&HF)
                AddCodeByte(&HD5) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HD5) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'psubd operando1, operando2
    'reg,[reg/mem] o [reg/mem],reg o reg,reg 011001100000111111111010 mod reg r/m scale index base displow disphigh 
    Public Sub psubdtobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Size = ParamSize.Bits128 Then AddCodeByte(&H66)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro 
                AddCodeByte(&HF)
                AddCodeByte(&HFA) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HFA) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'addps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011000 mod reg r/m scale index base displow disphigh 
    Public Sub addpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H58) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H58) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'andps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101010100 mod reg r/m scale index base displow disphigh 
    Public Sub andpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H54) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H54) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'cmpps operando1, operando2
    'reg,[reg/mem]/reg,imm8 o reg,reg 00001111 11000010 mod reg r/m scale index base displow disphigh imm
    Public Sub cmppstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&HC2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HC2) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
            AddCodeByte(op3.Value) 'carica l'immediato
        End If
    End Sub

    'cvtsi2ss operando1, operando2
    'reg,[reg/mem] o reg,reg 111100110000111100101010 mod reg r/m scale index base displow disphigh 
    Public Sub cvtsi2sstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamReg Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF3)
                AddCodeByte(&HF)
                AddCodeByte(&H2A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), RegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF3)
                AddCodeByte(&HF)
                AddCodeByte(&H2A) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'divps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011110 mod reg r/m scale index base displow disphigh 
    Public Sub divpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5E) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5E) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'maxps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011111 mod reg r/m scale index base displow disphigh 
    Public Sub maxpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5F) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5F) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'minps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011101 mod reg r/m scale index base displow disphigh 
    Public Sub minpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5D) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5D) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'movaps operando1, operando2
    'reg,[reg/mem] o reg,reg 000011110010100d mod reg r/m scale index base displow disphigh 
    Public Sub movapstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H28) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H28) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'movups operando1, operando2
    'reg,[reg/mem] o reg,reg 000011110001000d mod reg r/m scale index base displow disphigh 
    Public Sub movupstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)

        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H10) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67)
                AddCodeByte(&HF)
                AddCodeByte(&H10) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If

        ElseIf op1.Type = ParamType.ParamMem Then
            If op2.Type = ParamType.ParamMM Then
                If Is16BitAddressing(op1.Value.ToString) Then AddCodeByte(&H67)
                Dim OpCode1 As Byte = SetBit(&H10, 0)
                AddCodeByte(&HF)
                AddCodeByte(OpCode1) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op1.Value.ToString, MMRegToBin(op2.Value))
            End If
        End If
    End Sub

    'mulps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011001 mod reg r/m scale index base displow disphigh 
    Public Sub mulpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H59) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H59) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'orps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011010 mod reg r/m scale index base displow disphigh 
    Public Sub orpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H56) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H56) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'shufps operando1, operando2
    'reg,[reg/mem]/reg,imm8 o reg,reg 0000111111000110 mod reg r/m scale index base displow disphigh imm
    Public Sub shufpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&HC6) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&HC6) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
            AddCodeByte(op3.Value) 'carica l'immediato
        End If
    End Sub

    'subps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101011100 mod reg r/m scale index base displow disphigh 
    Public Sub subpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H5C) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H5C) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'unpckhps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111100010101 mod reg r/m scale index base displow disphigh 
    Public Sub unpckhpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H15) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H15) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'unpcklps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111100010100 mod reg r/m scale index base displow disphigh 
    Public Sub unpcklpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H14) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H14) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    'xorps operando1, operando2
    'reg,[reg/mem] o reg,reg 0000111101010111 mod reg r/m scale index base displow disphigh 
    Public Sub xorpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H57) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H57) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
        End If
    End Sub

    Public Sub extractpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        AddCodeByte(&H66)
        If op1.Type = ParamType.ParamReg Then 'se il primo operando è tipo registro
            AddCodeByte(&HF)
            AddCodeByte(&H17) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            AddCodeByte(EncodeModRM(&H3, RegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm
        ElseIf op1.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
            If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
            AddCodeByte(&HF)
            AddCodeByte(&H17) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
            WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
        End If
        AddCodeByte(op3.Value) 'carica l'immediato
    End Sub

    Public Sub insertpstobin(ByVal op As ParsedInstruction)
        Dim op1 As ParsedOperand = op.Operands(0)
        Dim op2 As ParsedOperand = op.Operands(1)
        Dim op3 As ParsedOperand = op.Operands(2)
        AddCodeByte(&H66)
        If op1.Type = ParamType.ParamMM Then 'se il primo operando è tipo registro
            If op2.Type = ParamType.ParamMM Then 'se il secondo operando è tipo registro
                AddCodeByte(&HF)
                AddCodeByte(&H21) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                AddCodeByte(EncodeModRM(&H3, MMRegToBin(op1.Value), MMRegToBin(op2.Value))) 'carica il mod reg rm

            ElseIf op2.Type = ParamType.ParamMem Then 'se il secondo operando è di tipo memoria
                If Is16BitAddressing(op2.Value.ToString) Then AddCodeByte(&H67) 'se si trata di un indirizzamento a 16 bit setta il prefisso address override
                AddCodeByte(&HF)
                AddCodeByte(&H21) 'carica l'opcode dell'istruzione, con set del bit d se non è una operazione a 8 bit
                WriteMem(op2.Value.ToString, MMRegToBin(op1.Value))
            End If
            AddCodeByte(op3.Value) 'carica l'immediato
        End If
    End Sub

End Module
