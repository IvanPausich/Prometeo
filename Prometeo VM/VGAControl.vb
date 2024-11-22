Imports System.IO
Imports System.Windows.Forms

Public Class VGAControl
    Inherits Form

    Private WithEvents Dma_I_O As GestioneI_O
    Private WithEvents cpuTimer As New Timer()
    Private WithEvents updateTimer As New Timer()
    Private _path As String
    Private mainMenu As MenuStrip
    Private isPaused As Boolean = False

    ' Riferimenti agli elementi del menu
    Private loadProgramMenuItem As ToolStripMenuItem
    Private startMenuItem As ToolStripMenuItem
    Private pauseMenuItem As ToolStripMenuItem
    Private stopMenuItem As ToolStripMenuItem
    Private ResetMenuItem As ToolStripMenuItem

    Public Sub New()
        InitializeComponent()
        Me.Text = "Prometeo Virtual Machine"
        Me.DoubleBuffered = True

        ' Disabilita il ridimensionamento del form
        Me.FormBorderStyle = FormBorderStyle.FixedSingle
        Me.MaximizeBox = False

        ' Crea il menu
        CreateMenu()

        ' Imposta lo stato iniziale del menu
        UpdateMenuState()
    End Sub

    Private Sub CreateMenu()
        mainMenu = New MenuStrip()
        Me.MainMenuStrip = mainMenu

        ' Menu File
        Dim fileMenu = New ToolStripMenuItem("File")
        loadProgramMenuItem = New ToolStripMenuItem("Load Program", Nothing, AddressOf LoadProgramMenuItem_Click)
        fileMenu.DropDownItems.Add(loadProgramMenuItem)
        fileMenu.DropDownItems.Add("-", Nothing, Nothing)
        fileMenu.DropDownItems.Add("Esci", Nothing, AddressOf ExitMenuItem_Click)
        mainMenu.Items.Add(fileMenu)

        ' Menu Execution
        Dim executionMenu = New ToolStripMenuItem("Execution")
        startMenuItem = New ToolStripMenuItem("Start", Nothing, AddressOf StartMenuItem_Click)
        pauseMenuItem = New ToolStripMenuItem("Pause", Nothing, AddressOf PauseMenuItem_Click)
        stopMenuItem = New ToolStripMenuItem("Stop", Nothing, AddressOf StopMenuItem_Click)
        ResetMenuItem = New ToolStripMenuItem("Reset", Nothing, AddressOf resetMenuItem_Click)
        executionMenu.DropDownItems.Add(startMenuItem)
        executionMenu.DropDownItems.Add(pauseMenuItem)
        executionMenu.DropDownItems.Add(stopMenuItem)
        executionMenu.DropDownItems.Add("-", Nothing, Nothing)
        executionMenu.DropDownItems.Add(ResetMenuItem)
        mainMenu.Items.Add(executionMenu)

        ' Aggiungi il menu al form
        Me.Controls.Add(mainMenu)
    End Sub

    Private Sub UpdateMenuState()
        Dim programLoaded As Boolean = (Dma_I_O IsNot Nothing)
        Dim isRunning As Boolean = programLoaded AndAlso cpuTimer.Enabled

        loadProgramMenuItem.Enabled = Not isRunning
        startMenuItem.Enabled = programLoaded AndAlso Not isRunning
        pauseMenuItem.Enabled = isRunning
        stopMenuItem.Enabled = isRunning OrElse isPaused
        startMenuItem.Enabled = programLoaded AndAlso Not isRunning
        ' Aggiorna il testo del menu Pause
        pauseMenuItem.Text = If(isPaused, "Resume", "Pause")
    End Sub

    Private Sub LoadProgramMenuItem_Click(sender As Object, e As EventArgs)
        Dim openFileDialog As New OpenFileDialog()
        openFileDialog.Filter = "Program Files (*.prg)|*.prg"
        openFileDialog.Title = "Select a Program File"

        If openFileDialog.ShowDialog() = DialogResult.OK Then
            _path = Path.GetDirectoryName(openFileDialog.FileName)
            LoadProgramFile(openFileDialog.FileName)
            UpdateMenuState()
        End If
    End Sub

    Private Sub ExitMenuItem_Click(sender As Object, e As EventArgs)
        Me.Close()
    End Sub

    Private Sub StartMenuItem_Click(sender As Object, e As EventArgs)
        If Dma_I_O IsNot Nothing Then
            cpuTimer.Start()
            updateTimer.Start()
            isPaused = False
            UpdateMenuState()
        Else
            MessageBox.Show("Please load a program first.", "No Program Loaded", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If
    End Sub

    Private Sub PauseMenuItem_Click(sender As Object, e As EventArgs)
        If Dma_I_O IsNot Nothing Then
            If isPaused Then
                cpuTimer.Start()
                updateTimer.Start()
                isPaused = False
            Else
                cpuTimer.Stop()
                updateTimer.Stop()
                isPaused = True
            End If
            UpdateMenuState()
        End If
    End Sub

    Private Sub StopMenuItem_Click(sender As Object, e As EventArgs)
        If Dma_I_O IsNot Nothing Then
            cpuTimer.Stop()
            updateTimer.Stop()
            isPaused = False
            ' Qui potresti voler aggiungere del codice per resettare lo stato della macchina virtuale
            UpdateMenuState()
        End If
    End Sub

    Private Sub ResetMenuItem_Click(sender As Object, e As EventArgs)
        If Dma_I_O IsNot Nothing Then
            cpuTimer.Stop()
            updateTimer.Stop()
            isPaused = False
            ' Qui potresti voler aggiungere del codice per resettare lo stato della macchina virtuale
            Dma_I_O.cpu.Reset()
            UpdateMenuState()
        End If
    End Sub

    Private Sub LoadProgramFile(filePath As String)
        Try
            Using reader As New BinaryReader(File.Open(filePath, FileMode.Open))
                ' Leggi le configurazioni
                Dim memSize As UInteger = reader.ReadUInt32()
                Dim memCacheSize As UInteger = reader.ReadUInt32()
                Dim vgaResolution As Byte = reader.ReadByte()
                Dim vgaWidth As UShort = reader.ReadUInt16()
                Dim vgaHeight As UShort = reader.ReadUInt16()
                Dim InterruptEnabled As Boolean = reader.ReadByte()

                ' Inizializza GestioneI_O con le configurazioni lette
                Dma_I_O = New GestioneI_O(memSize, memCacheSize, vgaResolution, _path)
                Dma_I_O.cpu.EnabledIRQ = InterruptEnabled
                ' Configura la VGA se è stata specificata una risoluzione personalizzata
                If vgaResolution = &H0 Then
                    Dma_I_O.VGA.Width = vgaWidth
                    Dma_I_O.VGA.Height = vgaHeight
                End If
                ' Adatta le dimensioni del form alla risoluzione VGA
                Me.ClientSize = New Size(Dma_I_O.VGA.Width, Dma_I_O.VGA.Height)

                ' Centra il form sullo schermo
                Me.StartPosition = FormStartPosition.CenterScreen

                ' Leggi il binario del programma
                Dim programData As Byte() = reader.ReadBytes(CInt(reader.BaseStream.Length - reader.BaseStream.Position))

                ' Carica il programma in memoria
                Dma_I_O.mem.WriteByteArray(programData, 0)

                ' Configura gli eventi
                AddHandler Me.KeyDown, AddressOf Dma_I_O.OnKeyDown
                AddHandler Me.MouseMove, AddressOf Dma_I_O.OnMouseMove
                AddHandler Me.MouseDown, AddressOf Dma_I_O.OnMouseDown
                AddHandler Me.MouseUp, AddressOf Dma_I_O.OnMouseUp

                ' Configura i timer
                updateTimer.Interval = 33 ' Circa 30 FPS
                AddHandler updateTimer.Tick, AddressOf Dma_I_O.UpdateScreen

                cpuTimer.Interval = 1 ' Esegue il più velocemente possibile
                AddHandler cpuTimer.Tick, AddressOf Dma_I_O.ExecuteCPUCycle

                UpdateMenuState()
            End Using

        Catch ex As Exception
            LogError($"Error loading program: {ex.Message}")
        End Try
    End Sub

    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        If Dma_I_O IsNot Nothing AndAlso Dma_I_O.VGA IsNot Nothing Then
            e.Graphics.DrawImage(Dma_I_O.VGA.GetVirtualScreen(), 0, 0)
        End If
    End Sub

    Private Sub Dma_I_O_End_Execute(sender As Object) Handles Dma_I_O.End_Execute
        cpuTimer.Stop()
        updateTimer.Stop()
        UpdateMenuState()
    End Sub

    Protected Overrides Sub OnFormClosing(e As FormClosingEventArgs)
        ' Ferma i timer e libera le risorse
        cpuTimer.Stop()
        updateTimer.Stop()
        If Dma_I_O IsNot Nothing Then
        End If
        MyBase.OnFormClosing(e)
    End Sub

End Class