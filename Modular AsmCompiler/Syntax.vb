Public Class Syntax
    Private Class Operand
        Public Property Type As ParamType
        Public Property Size As ParamSize
        Public Property Value As Object
    End Class

    Private Class InstructionConfiguration
        Public Property Operands As List(Of Operand)
        Public Property HasVariableOperands As Boolean
        Public ReadOnly Property HasOperands As Boolean
            Get
                Return Operands IsNot Nothing AndAlso (Operands.Count > 0 OrElse HasVariableOperands)
            End Get
        End Property
        Public ReadOnly Property OperandCount As Integer
            Get
                Return If(Operands Is Nothing, 0, Operands.Count)
            End Get
        End Property

        Public Sub New()
            Operands = New List(Of Operand)
            HasVariableOperands = False
        End Sub

        Public Sub New(hasOperands As Boolean, hasVariableOperands As Boolean)
            If hasOperands OrElse hasVariableOperands Then
                Operands = New List(Of Operand)
            Else
                Operands = Nothing
            End If
            hasVariableOperands = hasVariableOperands
        End Sub
    End Class

    Private Class Instruction
        Public Property Mnemonic As String
        Public Property Configurations As List(Of InstructionConfiguration)
        Public ReadOnly Property ConfigurationCount As Integer
            Get
                Return Configurations.Count
            End Get
        End Property

        Public Sub New()
            Configurations = New List(Of InstructionConfiguration)
        End Sub
    End Class

    Private Class _InstructionSet
        Private ReadOnly _instructions As Dictionary(Of String, Instruction)

        Public Sub New()
            _instructions = New Dictionary(Of String, Instruction)(StringComparer.OrdinalIgnoreCase)
        End Sub

        Public Sub AddInstruction(mnemonic As String)
            If Not _instructions.ContainsKey(mnemonic) Then
                _instructions.Add(mnemonic, New Instruction With {.Mnemonic = mnemonic})
            End If
        End Sub

        Public Sub AddInstructionConfiguration(mnemonic As String, operands As Operand(), Optional hasVariableOperands As Boolean = False)
            If Not _instructions.ContainsKey(mnemonic) Then
                AddInstruction(mnemonic)
            End If

            Dim instruction = _instructions(mnemonic)
            Dim config = New InstructionConfiguration()
            config.HasVariableOperands = hasVariableOperands

            If operands IsNot Nothing Then
                For Each operand In operands
                    config.Operands.Add(operand)
                Next
            End If

            instruction.Configurations.Add(config)

        End Sub

        Public Sub AddInstructionWithoutOperands(mnemonic As String)
            AddInstructionConfiguration(mnemonic, Nothing)
        End Sub

        Public Function FindInstruction(mnemonic As String) As Instruction
            If _instructions.ContainsKey(mnemonic) Then
                Return _instructions(mnemonic)
            End If
            Return Nothing
        End Function

        Public Function GetAllInstructions() As IEnumerable(Of Instruction)
            Return _instructions.Values
        End Function

        Public Function GetInstructionCount() As Integer
            Return _instructions.Count
        End Function
    End Class

    Private instructionSet As New _InstructionSet()

    ' Helper method to add CMOVcc configurations
    Private Sub AddCMOVccConfigurations(mnemonic As String)
        instructionSet.AddInstruction(mnemonic)

        ' 16-bit register to register
        instructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
             New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' 16-bit register to memory
        InstructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
             New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' 32-bit register to register
        InstructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
             New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' 32-bit register to memory
        InstructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
             New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})
    End Sub
    ' Helper method to add Jcc configurations
    Private Sub AddJccConfigurations(mnemonic As String)
        instructionSet.AddInstruction(mnemonic)

        ' 8-bit relative jump
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' 16/32-bit relative jump
        InstructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        InstructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
    End Sub

    Private Sub AddLoopConfiguration(mnemonic As String)
        instructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
    End Sub

    Public Sub Initializze_InstrList()

        ' Aggiunta di un'istruzione con un numero variabile di operandi (DB)
        instructionSet.AddInstructionConfiguration("DATA", Nothing, True)

        instructionSet.AddInstructionConfiguration("RESDATA",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("RESDATA",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("RESDATA",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("AAA")

        instructionSet.AddInstructionWithoutOperands("AAD")

        instructionSet.AddInstructionWithoutOperands("AAM")

        instructionSet.AddInstructionWithoutOperands("AAS")

        instructionSet.AddInstruction("ADC")
        ' ADC r8,imm8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADC m8,imm8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADC r16,imm16
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' ADC m16,imm16
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' ADC r32,imm32
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' ADC m32,imm32
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' ADC r16,imm8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADC m16,imm8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADC r32,imm8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADC m32,imm8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADC r8,r8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' ADC m8,r8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' ADC r16,r16
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' ADC m16,r16
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' ADC r32,r32
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' ADC m32,r32
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' ADC r8,m8
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' ADC r16,m16
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' ADC r32,m32
        instructionSet.AddInstructionConfiguration("ADC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        instructionSet.AddInstruction("ADD")

        ' ADD r8,imm8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADD m8,imm8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADD r16,imm16
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' ADD m16,imm16
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' ADD r32,imm32
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' ADD m32,imm32
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' ADD r16,imm8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADD m16,imm8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADD r32,imm8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADD m32,imm8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' ADD r8,r8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' ADD m8,r8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' ADD r16,r16
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' ADD m16,r16
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' ADD r32,r32
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' ADD m32,r32
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' ADD r8,m8
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' ADD r16,m16
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' ADD r32,m32
        instructionSet.AddInstructionConfiguration("ADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per ADDPD
        instructionSet.AddInstruction("ADDPD")

        ' ADDPD xmm1, xmm2
        instructionSet.AddInstructionConfiguration("ADDPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' ADDPD xmm1, m128
        instructionSet.AddInstructionConfiguration("ADDPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per ADDPS
        instructionSet.AddInstruction("ADDPS")

        ' ADDPS xmm1, xmm2
        instructionSet.AddInstructionConfiguration("ADDPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' ADDPS xmm1, m128
        instructionSet.AddInstructionConfiguration("ADDPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Aggiungiamo l'istruzione AND
        instructionSet.AddInstruction("AND")

        ' AND r8,imm8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' AND m8,imm8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' AND r16,imm16
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' AND m16,imm16
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' AND r32,imm32
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' AND m32,imm32
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' AND r16,imm8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' AND m16,imm8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' AND r32,imm8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' AND m32,imm8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' AND r8,r8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' AND m8,r8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' AND r16,r16
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' AND m16,r16
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' AND r32,r32
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' AND m32,r32
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' AND r8,m8
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' AND r16,m16
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' AND r32,m32
        instructionSet.AddInstructionConfiguration("AND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per ANDPD
        instructionSet.AddInstruction("ANDPD")

        instructionSet.AddInstructionConfiguration("ANDPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        instructionSet.AddInstructionConfiguration("ANDPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per ANDPS
        instructionSet.AddInstruction("ANDPS")

        instructionSet.AddInstructionConfiguration("ANDPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        instructionSet.AddInstructionConfiguration("ANDPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per BOUND
        instructionSet.AddInstruction("BOUND")

        instructionSet.AddInstructionConfiguration("BOUND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BOUND",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per BSF
        instructionSet.AddInstruction("BSF")

        instructionSet.AddInstructionConfiguration("BSF",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BSF",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BSF",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BSF",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per BSR
        instructionSet.AddInstruction("BSR")

        instructionSet.AddInstructionConfiguration("BSR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BSR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BSR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BSR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per BSWAP
        instructionSet.AddInstruction("BSWAP")

        instructionSet.AddInstructionConfiguration("BSWAP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' Configurazioni per BT
        instructionSet.AddInstruction("BT")

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurazioni per BTC
        instructionSet.AddInstruction("BTC")

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurazioni per BTR
        instructionSet.AddInstruction("BTR")

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurazioni per BTS
        instructionSet.AddInstruction("BTS")

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionConfiguration("BTS",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurazioni per CALL
        instructionSet.AddInstruction("CALL")

        instructionSet.AddInstructionConfiguration("CALL",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("CALL",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("CALL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("CALL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        instructionSet.AddInstructionConfiguration("CALL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionConfiguration("CALL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("CBW")

        instructionSet.AddInstructionWithoutOperands("CWDE")

        instructionSet.AddInstructionWithoutOperands("CLC")

        instructionSet.AddInstructionWithoutOperands("CLD")

        instructionSet.AddInstructionWithoutOperands("CLI")

        instructionSet.AddInstructionWithoutOperands("CMC")

        ' Add configurations for all CMOVcc instructions
        AddCMOVccConfigurations("CMOVA")
        AddCMOVccConfigurations("CMOVAE")
        AddCMOVccConfigurations("CMOVB")
        AddCMOVccConfigurations("CMOVBE")
        AddCMOVccConfigurations("CMOVC")
        AddCMOVccConfigurations("CMOVE")
        AddCMOVccConfigurations("CMOVG")
        AddCMOVccConfigurations("CMOVGE")
        AddCMOVccConfigurations("CMOVL")
        AddCMOVccConfigurations("CMOVLE")
        AddCMOVccConfigurations("CMOVNA")
        AddCMOVccConfigurations("CMOVNAE")
        AddCMOVccConfigurations("CMOVNB")
        AddCMOVccConfigurations("CMOVNBE")
        AddCMOVccConfigurations("CMOVNC")
        AddCMOVccConfigurations("CMOVNE")
        AddCMOVccConfigurations("CMOVNG")
        AddCMOVccConfigurations("CMOVNGE")
        AddCMOVccConfigurations("CMOVNL")
        AddCMOVccConfigurations("CMOVNLE")
        AddCMOVccConfigurations("CMOVNO")
        AddCMOVccConfigurations("CMOVNP")
        AddCMOVccConfigurations("CMOVNS")
        AddCMOVccConfigurations("CMOVNZ")
        AddCMOVccConfigurations("CMOVO")
        AddCMOVccConfigurations("CMOVP")
        AddCMOVccConfigurations("CMOVPE")
        AddCMOVccConfigurations("CMOVPO")
        AddCMOVccConfigurations("CMOVS")
        AddCMOVccConfigurations("CMOVZ")

        ' Configurazioni per CMP
        instructionSet.AddInstruction("CMP")

        ' CMP r/m8, imm8
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' CMP r/m16, imm16
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' CMP r/m32, imm32
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' CMP r/m16, imm8
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' CMP r/m32, imm8
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' CMP r/m8, r8
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' CMP r/m16, r16
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' CMP r/m32, r32
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' CMP r8, r/m8
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' CMP r16, r/m16
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' CMP r32, r/m32
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("CMP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per CMPPD
        instructionSet.AddInstruction("CMPPD")

        ' CMPPD xmm1, xmm2, imm8
        instructionSet.AddInstructionConfiguration("CMPPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' CMPPD xmm1, m128, imm8
        instructionSet.AddInstructionConfiguration("CMPPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurazioni per CMPPS
        instructionSet.AddInstruction("CMPPS")

        ' CMPPS xmm1, xmm2, imm8
        instructionSet.AddInstructionConfiguration("CMPPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' CMPPS xmm1, m128, imm8
        instructionSet.AddInstructionConfiguration("CMPPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionWithoutOperands("CMPSB")
        instructionSet.AddInstructionWithoutOperands("CMPSW")
        instructionSet.AddInstructionWithoutOperands("CMPSD")

        ' Configurazioni per CMPXCHG
        instructionSet.AddInstruction("CMPXCHG")

        ' CMPXCHG r/m8, r8
        instructionSet.AddInstructionConfiguration("CMPXCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("CMPXCHG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' CMPXCHG r/m16, r16
        instructionSet.AddInstructionConfiguration("CMPXCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("CMPXCHG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' CMPXCHG r/m32, r32
        instructionSet.AddInstructionConfiguration("CMPXCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("CMPXCHG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' Configurazioni per CVTPD2PS
        instructionSet.AddInstruction("CVTPD2PS")

        ' CVTPD2PS xmm1, xmm2
        instructionSet.AddInstructionConfiguration("CVTPD2PS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' CVTPD2PS xmm1, m128
        instructionSet.AddInstructionConfiguration("CVTPD2PS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per CVTPS2PD
        instructionSet.AddInstruction("CVTPS2PD")

        ' CVTPS2PD xmm1, xmm2
        instructionSet.AddInstructionConfiguration("CVTPS2PD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64}})

        ' CVTPS2PD xmm1, m64
        instructionSet.AddInstructionConfiguration("CVTPS2PD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' Configurazioni per CVTSI2SD
        instructionSet.AddInstruction("CVTSI2SD")

        ' CVTSI2SD xmm, r32
        instructionSet.AddInstructionConfiguration("CVTSI2SD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' CVTSI2SD xmm, m32
        instructionSet.AddInstructionConfiguration("CVTSI2SD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per CVTSI2SS
        instructionSet.AddInstruction("CVTSI2SS")

        ' CVTSI2SS xmm, r32
        instructionSet.AddInstructionConfiguration("CVTSI2SS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' CVTSI2SS xmm, m32
        instructionSet.AddInstructionConfiguration("CVTSI2SS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("CWD")
        instructionSet.AddInstructionWithoutOperands("CDQ")

        instructionSet.AddInstructionWithoutOperands("DAA")

        instructionSet.AddInstructionWithoutOperands("DAS")

        ' Configurazioni per DEC
        instructionSet.AddInstruction("DEC")

        ' DEC r/m8
        instructionSet.AddInstructionConfiguration("DEC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("DEC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' DEC r/m16
        instructionSet.AddInstructionConfiguration("DEC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("DEC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' DEC r/m32
        instructionSet.AddInstructionConfiguration("DEC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("DEC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per DIV
        instructionSet.AddInstruction("DIV")

        ' DIV r/m8
        instructionSet.AddInstructionConfiguration("DIV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("DIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' DIV r/m16
        instructionSet.AddInstructionConfiguration("DIV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("DIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' DIV r/m32
        instructionSet.AddInstructionConfiguration("DIV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("DIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per DIVPD
        instructionSet.AddInstruction("DIVPD")

        ' DIVPD xmm1, xmm2
        instructionSet.AddInstructionConfiguration("DIVPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' DIVPD xmm1, m128
        instructionSet.AddInstructionConfiguration("DIVPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per DIVPS
        instructionSet.AddInstruction("DIVPS")

        ' DIVPS xmm1, xmm2
        instructionSet.AddInstructionConfiguration("DIVPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' DIVPS xmm1, m128
        instructionSet.AddInstructionConfiguration("DIVPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per FADD
        instructionSet.AddInstruction("FADD")

        ' FADD m32fp
        instructionSet.AddInstructionConfiguration("FADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FADD m64fp
        instructionSet.AddInstructionConfiguration("FADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FADD ST(0), ST(i)
        instructionSet.AddInstructionConfiguration("FADD",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' FADD ST(i), ST(0)
        instructionSet.AddInstructionConfiguration("FADD",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0}})

        ' Configurazioni per FCOM
        instructionSet.AddInstruction("FCOM")

        ' FCOM m32fp
        instructionSet.AddInstructionConfiguration("FCOM",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FCOM m64fp
        instructionSet.AddInstructionConfiguration("FCOM",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FCOM ST(i)
        instructionSet.AddInstructionConfiguration("FCOM",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FCOMP
        instructionSet.AddInstruction("FCOMP")

        ' FCOMP m32fp
        instructionSet.AddInstructionConfiguration("FCOMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FCOMP m64fp
        instructionSet.AddInstructionConfiguration("FCOMP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FCOMP ST(i)
        instructionSet.AddInstructionConfiguration("FCOMP",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FCOMI
        instructionSet.AddInstruction("FCOMI")

        ' FCOMI ST, ST(i)
        instructionSet.AddInstructionConfiguration("FCOMI",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FCOMIP
        instructionSet.AddInstruction("FCOMIP")

        ' FCOMIP ST, ST(i)
        instructionSet.AddInstructionConfiguration("FCOMIP",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FDIV
        instructionSet.AddInstruction("FDIV")

        ' FDIV m32fp
        instructionSet.AddInstructionConfiguration("FDIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FDIV m64fp
        instructionSet.AddInstructionConfiguration("FDIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FDIV ST(0), ST(i)
        instructionSet.AddInstructionConfiguration("FDIV",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' FDIV ST(i), ST(0)
        instructionSet.AddInstructionConfiguration("FDIV",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0}})

        ' Configurazioni per FLD
        instructionSet.AddInstruction("FLD")

        ' FLD m32fp
        instructionSet.AddInstructionConfiguration("FLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FLD m64fp
        instructionSet.AddInstructionConfiguration("FLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FLD m80fp
        instructionSet.AddInstructionConfiguration("FLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits80}})

        ' FLD ST(i)
        instructionSet.AddInstructionConfiguration("FLD",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FMUL
        instructionSet.AddInstruction("FMUL")

        ' FMUL m32fp
        instructionSet.AddInstructionConfiguration("FMUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FMUL m64fp
        instructionSet.AddInstructionConfiguration("FMUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FMUL ST(0), ST(i)
        instructionSet.AddInstructionConfiguration("FMUL",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' FMUL ST(i), ST(0)
        instructionSet.AddInstructionConfiguration("FMUL",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0}})

        ' Configurazioni per FSCALE
        instructionSet.AddInstructionWithoutOperands("FSCALE")

        ' Configurazioni per FSQRT
        instructionSet.AddInstructionWithoutOperands("FSQRT")

        ' Configurazioni per FST
        instructionSet.AddInstruction("FST")

        ' FST m32fp
        instructionSet.AddInstructionConfiguration("FST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FST m64fp
        instructionSet.AddInstructionConfiguration("FST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FST ST(i)
        instructionSet.AddInstructionConfiguration("FST",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FSTP
        instructionSet.AddInstruction("FSTP")

        ' FSTP m32fp
        instructionSet.AddInstructionConfiguration("FSTP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FSTP m64fp
        instructionSet.AddInstructionConfiguration("FSTP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FSTP m80fp
        instructionSet.AddInstructionConfiguration("FSTP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits80}})

        ' FSTP ST(i)
        instructionSet.AddInstructionConfiguration("FSTP",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' Configurazioni per FSUB
        instructionSet.AddInstruction("FSUB")

        ' FSUB m32fp
        instructionSet.AddInstructionConfiguration("FSUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' FSUB m64fp
        instructionSet.AddInstructionConfiguration("FSUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})

        ' FSUB ST(0), ST(i)
        instructionSet.AddInstructionConfiguration("FSUB",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        ' FSUB ST(i), ST(0)
        instructionSet.AddInstructionConfiguration("FSUB",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80},
     New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80, .Value = ASMFPURegisters.FP_ST0}})

        ' Configurazioni per FXCH
        instructionSet.AddInstruction("FXCH")

        ' FXCH ST(i)
        instructionSet.AddInstructionConfiguration("FXCH",
    {New Operand With {.Type = ParamType.ParamSTX, .Size = ParamSize.Bits80}})

        instructionSet.AddInstructionWithoutOperands("HLT")

        ' Configurazioni per IDIV
        instructionSet.AddInstruction("IDIV")

        ' IDIV r/m8
        instructionSet.AddInstructionConfiguration("IDIV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("IDIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' IDIV r/m16
        instructionSet.AddInstructionConfiguration("IDIV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("IDIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' IDIV r/m32
        instructionSet.AddInstructionConfiguration("IDIV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("IDIV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per IMUL
        instructionSet.AddInstruction("IMUL")

        ' IMUL r/m8
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' IMUL r/m16
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' IMUL r/m32
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' IMUL r16,r/m16
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' IMUL r32,r/m32
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' IMUL r16,r/m16,imm8
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' IMUL r32,r/m32,imm8
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' IMUL r16,r/m16,imm16
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' IMUL r32,r/m32,imm32
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("IMUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' Configurazioni per IN
        instructionSet.AddInstruction("IN")

        ' IN AL, imm8
        instructionSet.AddInstructionConfiguration("IN",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegAL},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' IN AX, imm8
        instructionSet.AddInstructionConfiguration("IN",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegAX},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' IN EAX, imm8
        instructionSet.AddInstructionConfiguration("IN",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32, .Value = ASMRegisters.RegEAX},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' IN AL, DX
        instructionSet.AddInstructionConfiguration("IN",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegAL},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegDX}})

        ' IN AX, DX
        instructionSet.AddInstructionConfiguration("IN",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegAX},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegDX}})

        ' IN EAX, DX
        instructionSet.AddInstructionConfiguration("IN",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32, .Value = ASMRegisters.RegEAX},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegDX}})

        ' Configurazioni per INC
        instructionSet.AddInstruction("INC")

        ' INC r/m8
        instructionSet.AddInstructionConfiguration("INC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("INC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' INC r/m16
        instructionSet.AddInstructionConfiguration("INC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("INC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' INC r/m32
        instructionSet.AddInstructionConfiguration("INC",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("INC",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("INSB")
        instructionSet.AddInstructionWithoutOperands("INSW")
        instructionSet.AddInstructionWithoutOperands("INSD")

        instructionSet.AddInstructionWithoutOperands("IRET")


        ' Add configurations for all Jcc instructions
        AddJccConfigurations("JA")
        AddJccConfigurations("JAE")
        AddJccConfigurations("JB")
        AddJccConfigurations("JBE")
        AddJccConfigurations("JC")
        AddJccConfigurations("JCXZ")
        AddJccConfigurations("JECXZ")
        AddJccConfigurations("JE")
        AddJccConfigurations("JG")
        AddJccConfigurations("JGE")
        AddJccConfigurations("JL")
        AddJccConfigurations("JLE")
        AddJccConfigurations("JNA")
        AddJccConfigurations("JNAE")
        AddJccConfigurations("JNB")
        AddJccConfigurations("JNBE")
        AddJccConfigurations("JNC")
        AddJccConfigurations("JNE")
        AddJccConfigurations("JNG")
        AddJccConfigurations("JNGE")
        AddJccConfigurations("JNL")
        AddJccConfigurations("JNLE")
        AddJccConfigurations("JNO")
        AddJccConfigurations("JNP")
        AddJccConfigurations("JNS")
        AddJccConfigurations("JNZ")
        AddJccConfigurations("JO")
        AddJccConfigurations("JP")
        AddJccConfigurations("JPE")
        AddJccConfigurations("JPO")
        AddJccConfigurations("JS")
        AddJccConfigurations("JZ")

        ' Special cases for JCXZ and JECXZ (only 8-bit relative jump)
        instructionSet.AddInstruction("JCXZ")
        instructionSet.AddInstructionConfiguration("JCXZ",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("JCXZ",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        instructionSet.AddInstruction("JECXZ")
        instructionSet.AddInstructionConfiguration("JECXZ",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("JECXZ",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        instructionSet.AddInstruction("LIA")
        instructionSet.AddInstructionConfiguration("LIA",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' Configurazioni per JMP
        instructionSet.AddInstruction("JMP")

        ' JMP rel8
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' JMP rel16
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' JMP rel32
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' JMP r/m16
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' JMP r/m32
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("JMP",
            {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per LEA
        instructionSet.AddInstruction("LEA")

        ' LEA r16,m
        instructionSet.AddInstructionConfiguration("LEA",
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
             New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.BitsUnknown}})

        ' LEA r32,m
        instructionSet.AddInstructionConfiguration("LEA",
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
             New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.BitsUnknown}})

        ' Configurazioni per LOOP/LOOPcc
        instructionSet.AddInstruction("LOOP")
        instructionSet.AddInstruction("LOOPE")
        instructionSet.AddInstruction("LOOPZ")
        instructionSet.AddInstruction("LOOPNE")
        instructionSet.AddInstruction("LOOPNZ")

        ' Helper function for LOOP instructions

        AddLoopConfiguration("LOOP")
        AddLoopConfiguration("LOOPE")
        AddLoopConfiguration("LOOPZ")
        AddLoopConfiguration("LOOPNE")
        AddLoopConfiguration("LOOPNZ")

        ' Configurazioni per MAXPD
        instructionSet.AddInstruction("MAXPD")

        ' MAXPD xmm1, xmm2
        instructionSet.AddInstructionConfiguration("MAXPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' MAXPD xmm1, m128
        instructionSet.AddInstructionConfiguration("MAXPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per MAXPS
        instructionSet.AddInstruction("MAXPS")

        ' MAXPS xmm1, xmm2
        instructionSet.AddInstructionConfiguration("MAXPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' MAXPS xmm1, m128
        instructionSet.AddInstructionConfiguration("MAXPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per MINPD
        instructionSet.AddInstruction("MINPD")

        ' MINPD xmm1, xmm2
        instructionSet.AddInstructionConfiguration("MINPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' MINPD xmm1, m128
        instructionSet.AddInstructionConfiguration("MINPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per MINPS
        instructionSet.AddInstruction("MINPS")

        ' MINPS xmm1, xmm2
        instructionSet.AddInstructionConfiguration("MINPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' MINPS xmm1, m128
        instructionSet.AddInstructionConfiguration("MINPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        instructionSet.AddInstructionWithoutOperands("LODSB")
        instructionSet.AddInstructionWithoutOperands("LODSW")
        instructionSet.AddInstructionWithoutOperands("LODSD")
        instructionSet.AddInstructionWithoutOperands("LAHF")


        ' Configurazioni per MOV
        instructionSet.AddInstruction("MOV")

        ' MOV r/m8, r8
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' MOV r/m16, r16
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' MOV r/m32, r32
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' MOV r8, r/m8
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' MOV r16, r/m16
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' MOV r32, r/m32
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' MOV r/m8, imm8
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' MOV r/m16, imm16
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' MOV r/m32, imm32
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("MOV",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("NOP")

        instructionSet.AddInstructionWithoutOperands("MOVSB")
        instructionSet.AddInstructionWithoutOperands("MOVSW")
        instructionSet.AddInstructionWithoutOperands("MOVSD")

        ' Configurazioni per MOVAPD
        instructionSet.AddInstruction("MOVAPD")
        instructionSet.AddInstructionConfiguration("MOVAPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MOVAPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per MOVAPS
        instructionSet.AddInstruction("MOVAPS")
        instructionSet.AddInstructionConfiguration("MOVAPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MOVAPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per MOVS/MOVSB/MOVSW/MOVSD
        instructionSet.AddInstruction("MOVSB")
        instructionSet.AddInstructionConfiguration("MOVSB", {})
        instructionSet.AddInstruction("MOVSW")
        instructionSet.AddInstructionConfiguration("MOVSW", {})
        instructionSet.AddInstruction("MOVSD")
        instructionSet.AddInstructionConfiguration("MOVSD", {})

        ' Configurazioni per MOVSX
        instructionSet.AddInstruction("MOVSX")
        instructionSet.AddInstructionConfiguration("MOVSX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVSX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVSX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVSX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVSX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MOVSX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' Configurazioni per MOVUPD
        instructionSet.AddInstruction("MOVUPD")
        instructionSet.AddInstructionConfiguration("MOVUPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MOVUPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MOVUPD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' Configurazioni per MOVUPS
        instructionSet.AddInstruction("MOVUPS")
        instructionSet.AddInstructionConfiguration("MOVUPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MOVUPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MOVUPS",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})

        ' Configurazioni per MOVZX
        instructionSet.AddInstruction("MOVZX")
        instructionSet.AddInstructionConfiguration("MOVZX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVZX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVZX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVZX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MOVZX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MOVZX",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' Configurazioni per MUL
        instructionSet.AddInstruction("MUL")
        instructionSet.AddInstructionConfiguration("MUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("MUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("MUL",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("MUL",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per MULPD
        instructionSet.AddInstruction("MULPD")
        instructionSet.AddInstructionConfiguration("MULPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MULPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per MULPS
        instructionSet.AddInstruction("MULPS")
        instructionSet.AddInstructionConfiguration("MULPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("MULPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per NEG
        instructionSet.AddInstruction("NEG")
        instructionSet.AddInstructionConfiguration("NEG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("NEG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("NEG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("NEG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("NEG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("NEG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per NOT
        instructionSet.AddInstruction("NOT")
        instructionSet.AddInstructionConfiguration("NOT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("NOT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("NOT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("NOT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("NOT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("NOT",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per OR
        instructionSet.AddInstruction("OR")
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("OR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per ORPD
        instructionSet.AddInstruction("ORPD")
        instructionSet.AddInstructionConfiguration("ORPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("ORPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per ORPS
        instructionSet.AddInstruction("ORPS")
        instructionSet.AddInstructionConfiguration("ORPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("ORPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per OUT
        instructionSet.AddInstruction("OUT")
        instructionSet.AddInstructionConfiguration("OUT",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegAL}})
        instructionSet.AddInstructionConfiguration("OUT",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegAX}})
        instructionSet.AddInstructionConfiguration("OUT",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32, .Value = ASMRegisters.RegEAX}})
        instructionSet.AddInstructionConfiguration("OUT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegDX},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegAL}})
        instructionSet.AddInstructionConfiguration("OUT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegDX},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegAX}})
        instructionSet.AddInstructionConfiguration("OUT",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16, .Value = ASMRegisters.RegDX},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32, .Value = ASMRegisters.RegEAX}})

        instructionSet.AddInstructionWithoutOperands("OUTSB")
        instructionSet.AddInstructionWithoutOperands("OUTSW")
        instructionSet.AddInstructionWithoutOperands("OUTSD")
        instructionSet.AddInstructionWithoutOperands("POPA")
        instructionSet.AddInstructionWithoutOperands("POPAD")

        instructionSet.AddInstructionWithoutOperands("POPF")
        instructionSet.AddInstructionWithoutOperands("POPFD")

        instructionSet.AddInstructionWithoutOperands("PUSHA")
        instructionSet.AddInstructionWithoutOperands("PUSHAD")

        instructionSet.AddInstructionWithoutOperands("PUSHF")
        instructionSet.AddInstructionWithoutOperands("PUSHFD")


        ' Configurazioni per PADDD
        instructionSet.AddInstruction("PADDD")
        instructionSet.AddInstructionConfiguration("PADDD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64}})
        instructionSet.AddInstructionConfiguration("PADDD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})
        instructionSet.AddInstructionConfiguration("PADDD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("PADDD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per PMULLW
        instructionSet.AddInstruction("PMULLW")
        instructionSet.AddInstructionConfiguration("PMULLW",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64}})
        instructionSet.AddInstructionConfiguration("PMULLW",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})
        instructionSet.AddInstructionConfiguration("PMULLW",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("PMULLW",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per POP
        instructionSet.AddInstruction("POP")
        instructionSet.AddInstructionConfiguration("POP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("POP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("POP",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("POP",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per PSUBD
        instructionSet.AddInstruction("PSUBD")
        instructionSet.AddInstructionConfiguration("PSUBD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64}})
        instructionSet.AddInstructionConfiguration("PSUBD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits64},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits64}})
        instructionSet.AddInstructionConfiguration("PSUBD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("PSUBD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per PUSH
        instructionSet.AddInstruction("PUSH")
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("PUSH",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' Add configurations for RCL, RCR, ROL, ROR
        AddRotationInstructionConfigurations("RCL")
        AddRotationInstructionConfigurations("RCR")
        AddRotationInstructionConfigurations("ROL")
        AddRotationInstructionConfigurations("ROR")

        instructionSet.AddInstructionWithoutOperands("REP")
        instructionSet.AddInstructionWithoutOperands("REPE")
        instructionSet.AddInstructionWithoutOperands("REPNE")
        instructionSet.AddInstructionWithoutOperands("SAHF")

        ' Configurazioni per RET
        instructionSet.AddInstruction("RET")
        instructionSet.AddInstructionConfiguration("RET", {})
        instructionSet.AddInstructionConfiguration("RET",
    {New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        ' Add configurations for SAL, SAR, SHL, SHR
        AddShiftInstructionConfigurations("SAL")
        AddShiftInstructionConfigurations("SAR")
        AddShiftInstructionConfigurations("SHL")
        AddShiftInstructionConfigurations("SHR")

        ' Configurações para SBB
        instructionSet.AddInstruction("SBB")

        ' SBB r/m8, imm8
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' SBB r/m16, imm16
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' SBB r/m32, imm32
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' SBB r/m16, imm8
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' SBB r/m32, imm8
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' SBB r/m8, r8
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' SBB r/m16, r16
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' SBB r/m32, r32
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' SBB r8, r/m8
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' SBB r16, r/m16
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' SBB r32, r/m32
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("SBB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("SCASB")
        instructionSet.AddInstructionWithoutOperands("SCASW")
        instructionSet.AddInstructionWithoutOperands("SCASD")


        ' Add configurations for all SETcc instructions
        AddSetccConfiguration("SETA")
        AddSetccConfiguration("SETAE")
        AddSetccConfiguration("SETB")
        AddSetccConfiguration("SETBE")
        AddSetccConfiguration("SETC")
        AddSetccConfiguration("SETE")
        AddSetccConfiguration("SETG")
        AddSetccConfiguration("SETGE")
        AddSetccConfiguration("SETL")
        AddSetccConfiguration("SETLE")
        AddSetccConfiguration("SETNA")
        AddSetccConfiguration("SETNAE")
        AddSetccConfiguration("SETNB")
        AddSetccConfiguration("SETNBE")
        AddSetccConfiguration("SETNC")
        AddSetccConfiguration("SETNE")
        AddSetccConfiguration("SETNG")
        AddSetccConfiguration("SETNGE")
        AddSetccConfiguration("SETNL")
        AddSetccConfiguration("SETNLE")
        AddSetccConfiguration("SETNO")
        AddSetccConfiguration("SETNP")
        AddSetccConfiguration("SETNS")
        AddSetccConfiguration("SETNZ")
        AddSetccConfiguration("SETO")
        AddSetccConfiguration("SETP")
        AddSetccConfiguration("SETPE")
        AddSetccConfiguration("SETPO")
        AddSetccConfiguration("SETS")
        AddSetccConfiguration("SETZ")

        ' Configurations for SHLD
        instructionSet.AddInstruction("SHLD")
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration("SHLD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})

        ' Configurations for SHRD
        instructionSet.AddInstruction("SHRD")
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration("SHRD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})

        ' Configurations for SHUFPD
        instructionSet.AddInstruction("SHUFPD")
        instructionSet.AddInstructionConfiguration("SHUFPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHUFPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurations for SHUFPS
        instructionSet.AddInstruction("SHUFPS")
        instructionSet.AddInstructionConfiguration("SHUFPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SHUFPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        instructionSet.AddInstructionWithoutOperands("STC")

        instructionSet.AddInstructionWithoutOperands("STD")

        instructionSet.AddInstructionWithoutOperands("STI")

        instructionSet.AddInstructionWithoutOperands("STOSB")
        instructionSet.AddInstructionWithoutOperands("STOSW")
        instructionSet.AddInstructionWithoutOperands("STOSD")

        ' Configurazioni per SUB
        instructionSet.AddInstruction("SUB")

        ' SUB r/m8, imm8
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' SUB r/m16, imm16
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' SUB r/m32, imm32
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' SUB r/m16, imm8
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' SUB r/m32, imm8
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' SUB r/m8, r8
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' SUB r/m16, r16
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' SUB r/m32, r32
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' SUB r8, r/m8
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' SUB r16, r/m16
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' SUB r32, r/m32
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("SUB",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurazioni per SUBPD
        instructionSet.AddInstruction("SUBPD")
        instructionSet.AddInstructionConfiguration("SUBPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("SUBPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni per SUBPS
        instructionSet.AddInstruction("SUBPS")
        instructionSet.AddInstructionConfiguration("SUBPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("SUBPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurazioni para TEST
        instructionSet.AddInstruction("TEST")

        ' TEST r/m8, imm8
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' TEST r/m16, imm16
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' TEST r/m32, imm32
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' TEST r/m8, r8
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' TEST r/m16, r16
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' TEST r/m32, r32
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("TEST",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' Configurações para UNPCKHPD
        instructionSet.AddInstruction("UNPCKHPD")
        instructionSet.AddInstructionConfiguration("UNPCKHPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("UNPCKHPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurações para UNPCKHPS
        instructionSet.AddInstruction("UNPCKHPS")
        instructionSet.AddInstructionConfiguration("UNPCKHPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("UNPCKHPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurações para UNPCKLPD
        instructionSet.AddInstruction("UNPCKLPD")
        instructionSet.AddInstructionConfiguration("UNPCKLPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("UNPCKLPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurações para UNPCKLPS
        instructionSet.AddInstruction("UNPCKLPS")
        instructionSet.AddInstructionConfiguration("UNPCKLPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("UNPCKLPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurações para XADD
        instructionSet.AddInstruction("XADD")

        ' XADD r/m8, r8
        instructionSet.AddInstructionConfiguration("XADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' XADD r/m16, r16
        instructionSet.AddInstructionConfiguration("XADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("XADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' XADD r/m32, r32
        instructionSet.AddInstructionConfiguration("XADD",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("XADD",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' Configurações para XCHG
        instructionSet.AddInstruction("XCHG")

        ' XCHG r/m8, r8
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' XCHG r8, r/m8
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' XCHG r/m16, r16
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' XCHG r16, r/m16
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' XCHG r/m32, r32
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' XCHG r32, r/m32
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("XCHG",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        instructionSet.AddInstructionWithoutOperands("XLATB")







        ' Configurações para XOR
        instructionSet.AddInstruction("XOR")

        ' XOR r/m8, imm8
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' XOR r/m16, imm16
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits16}})

        ' XOR r/m32, imm32
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits32}})

        ' XOR r/m16, imm8
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' XOR r/m32, imm8
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' XOR r/m8, r8
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})

        ' XOR r/m16, r16
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})

        ' XOR r/m32, r32
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})

        ' XOR r8, r/m8
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})

        ' XOR r16, r/m16
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})

        ' XOR r32, r/m32
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration("XOR",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})

        ' Configurações para XORPD
        instructionSet.AddInstruction("XORPD")
        instructionSet.AddInstructionConfiguration("XORPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("XORPD",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurações para XORPS
        instructionSet.AddInstruction("XORPS")
        instructionSet.AddInstructionConfiguration("XORPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128}})
        instructionSet.AddInstructionConfiguration("XORPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits128}})

        ' Configurações para EXTRACTPS
        instructionSet.AddInstruction("EXTRACTPS")
        instructionSet.AddInstructionConfiguration("EXTRACTPS",
    {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("EXTRACTPS",
    {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Configurações para INSERTPS
        instructionSet.AddInstruction("INSERTPS")
        instructionSet.AddInstructionConfiguration("INSERTPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration("INSERTPS",
    {New Operand With {.Type = ParamType.ParamMM, .Size = ParamSize.Bits128},
     New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
     New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' Ricerca di un'istruzione
        Dim movInstruction = instructionSet.FindInstruction("MOV")
    End Sub

    Private Sub AddShiftInstructionConfigurations(mnemonic As String)
        instructionSet.AddInstruction(mnemonic)

        ' 8-bit shifts
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' 16-bit shifts
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' 32-bit shifts
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
    End Sub

    Private Sub AddRotationInstructionConfigurations(mnemonic As String)
        instructionSet.AddInstruction(mnemonic)

        ' 8-bit rotations
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8, .Value = 1}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8, .Value = 1}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' 16-bit rotations
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8, .Value = 1}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8, .Value = 1}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits16},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})

        ' 32-bit rotations
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8, .Value = 1}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8, .Value = 1}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8, .Value = ASMRegisters.RegCL}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
        {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits32},
         New Operand With {.Type = ParamType.ParamImm, .Size = ParamSize.Bits8}})
    End Sub

    Private Sub AddSetccConfiguration(mnemonic As String)
        instructionSet.AddInstruction(mnemonic)
        instructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamReg, .Size = ParamSize.Bits8}})
        instructionSet.AddInstructionConfiguration(mnemonic,
            {New Operand With {.Type = ParamType.ParamMem, .Size = ParamSize.Bits8}})
    End Sub

    Public Function ValidateInstruction(parsedInstruction As Object) As Boolean
        ' Cerca l'istruzione nel set di istruzioni
        Dim instruction As Instruction = instructionSet.FindInstruction(parsedInstruction.Mnemonic)

        If instruction Is Nothing Then
            ' L'istruzione non esiste nel set
            Return False
        End If
        ' Controlla ogni configurazione dell'istruzione
        For Each config In instruction.Configurations
            If IsValidConfiguration(parsedInstruction, config) Then
                Return True
            End If
        Next

        ' Nessuna configurazione valida trovata
        Return False
    End Function

    Private Function IsValidConfiguration(parsedInstruction As ParsedInstruction, config As InstructionConfiguration) As Boolean
        ' Verifica il numero di operandi
        If parsedInstruction.Operands.Count <> config.OperandCount AndAlso Not config.HasVariableOperands Then
            ' Se il numero di operandi non corrisponde e la configurazione non ammette un numero variabile di operandi,
            ' passiamo alla configurazione successiva

            Return False
        End If

        ' Confronta ogni operando
        For i As Integer = 0 To Math.Min(parsedInstruction.Operands.Count - 1, config.OperandCount - 1)
            Dim parsedOperand As ParsedOperand = parsedInstruction.Operands(i)
            Dim configOperand As Operand = config.Operands(i)            ' Verifica il tipo dell'operando
            If parsedOperand.Type <> configOperand.Type Then
                If parsedOperand.Type <> ParamType.ParamRel AndAlso configOperand.Type <> ParamType.ParamImm Then Return False
            End If

            ' Verifica la dimensione dell'operando, se specificata
            If parsedOperand.Size <> configOperand.Size Then
                If parsedOperand.Size <> ParamSize.BitsUnknown Then Return False
            End If
            ' Verifica il valore specifico se presente nella configurazione
            If configOperand.Value IsNot Nothing Then
                If Not configOperand.Value.Equals(parsedOperand.Value) Then Return False
            End If
        Next

        ' Se siamo arrivati qui, tutti gli operandi corrispondono
        Return True
    End Function

End Class
