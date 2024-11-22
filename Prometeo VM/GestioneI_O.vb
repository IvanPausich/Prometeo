
Imports System.IO
Imports System.Runtime.InteropServices

Public Class GestioneI_O

    Private lastMouseX As Integer
    Private lastMouseY As Integer
    Public VGA As VirtualMonitor
    Public WithEvents cpu As VCPU
    Public mem As VMem
    Private VHD As VirtualHardDisk
    Private Mouse_Key As New LowLevelInput
    Private networkManager As New NetworkManager()

    Public Event End_Execute(sender As Object)
    ' Costanti per la gestione della memoria
    Private Const MIN_MEMORY_SIZE As UInteger = 256 * 1024 * 1024  ' 256 MB
    Private Const MAX_MEMORY_SIZE As UInteger = 1024 * 1024 * 2047  ' 2 GB
    Private serialManager As New USBSerialManager ' Assumiamo COM1 e 9600 baud come default
    Private networkMemSize As UInteger = 1024 * 1024 ' 1 MB per il buffer di rete
    Private soundCard As New SoundCardManager()
    Private loadedDlls As New Dictionary(Of String, IntPtr)
    Private inputFunctions As New Dictionary(Of String, InputFunction)
    Private outputFunctions As New Dictionary(Of String, OutputFunction)
    Private realFileSystem As New RealFileSystem()
    Private virtualClipboard As New VirtualClipboard()
    Private virtualSystemInfo As New VirtualSystemInfo()
    Private virtualEmailManager As VirtualEmailManager

    Private Delegate Function InputFunction(port As UShort, size As Byte) As UInteger
    Private Delegate Sub OutputFunction(port As UShort, size As Byte, value As UInteger)
    Private ExtraDevices As New List(Of Device)

    ' Struttura per rappresentare un dispositivo
    Private Structure Device
        Public Name As String
        Public InputPorts As List(Of UShort)
        Public OutputPorts As List(Of UShort)
        Public MemoryAllocation As UInteger
        Public DmaAddress As UInteger?
    End Structure

    ' Lista dei dispositivi
    Private Devices As New List(Of Device)


    ' Costruttore della classe
    Public Sub New(memorySize As UInteger, memoryCache As UInteger, resolution As Byte, path As String)
        ' Verifica e imposta la dimensione della memoria
        If memorySize < MIN_MEMORY_SIZE Then
            memorySize = MIN_MEMORY_SIZE
        ElseIf memorySize > MAX_MEMORY_SIZE Then
            memorySize = MAX_MEMORY_SIZE
        End If

        ' Inizializza i componenti
        mem = New VMem(path & "\Mem.bin", memorySize, memoryCache)
        cpu = New VCPU(mem)
        VGA = New VirtualMonitor(resolution)
        VHD = New VirtualHardDisk(path & "\HD.bin")

        ' Inizializza la lista dei dispositivi
        InitializeDevices(memorySize)
    End Sub

    Private Sub InitializeDevices(memorySize As UInteger)
        ' Calcoliamo le dimensioni per ciascun dispositivo
        Dim vgaMemSize As UInteger = VGA.Width * VGA.Height * 3  ' 3 byte per pixel (RGB)
        Dim hdMemSize As UInteger = 1024 * 1024 * 50 ' Usa la dimensione effettiva del disco virtuale
        virtualEmailManager = New VirtualEmailManager("smtp.example.com", 587, "username", "password", "sender@example.com")

        ' Calcoliamo la memoria totale richiesta dai dispositivi
        Dim totalDeviceMemory As UInteger = vgaMemSize + hdMemSize + networkMemSize

        ' Assicuriamoci che ci sia abbastanza memoria per tutti i dispositivi
        If totalDeviceMemory > memorySize Then
            LogError("Errore: Memoria insufficiente per allocare tutti i dispositivi")
        End If

        ' Calcoliamo la memoria rimanente per la RAM
        Dim ramSize As UInteger = memorySize - totalDeviceMemory

        ' Calcoliamo gli indirizzi DMA
        Dim currentAddress As UInteger = 0
        Dim ramAddress As UInteger = currentAddress
        currentAddress += ramSize
        Dim hdAddress As UInteger = currentAddress
        currentAddress += hdMemSize
        Dim networkAddress As UInteger = currentAddress
        currentAddress += networkMemSize
        Dim vgaAddress As UInteger = currentAddress

        ' Aggiungiamo i dispositivi con i nuovi indirizzi DMA
        Devices.Clear() ' Assicuriamoci che la lista sia vuota prima di aggiungere i nuovi dispositivi

        Devices.Add(New Device With {
        .Name = "RAM",
        .InputPorts = New List(Of UShort)(),
        .OutputPorts = New List(Of UShort)(),
        .MemoryAllocation = ramSize,
        .DmaAddress = ramAddress
    })

        Devices.Add(New Device With {
        .Name = "VGA",
        .InputPorts = New List(Of UShort)(),
        .OutputPorts = New List(Of UShort) From {&H3C0, &H3C1, &H3C2, &H3C3},
        .MemoryAllocation = vgaMemSize,
        .DmaAddress = vgaAddress
    })

        Devices.Add(New Device With {
        .Name = "HardDisk",
        .InputPorts = New List(Of UShort) From {&H1F0, &H1F1, &H1F2},
        .OutputPorts = New List(Of UShort) From {&H1F0, &H1F1, &H1F2},
        .MemoryAllocation = hdMemSize,
        .DmaAddress = hdAddress
    })

        Devices.Add(New Device With {
        .Name = "Keyboard",
        .InputPorts = New List(Of UShort) From {&H60},
        .OutputPorts = New List(Of UShort)(),
        .MemoryAllocation = 0,
        .DmaAddress = Nothing
    })

        Devices.Add(New Device With {
        .Name = "Mouse",
        .InputPorts = New List(Of UShort) From {&H60},
        .OutputPorts = New List(Of UShort)(),
        .MemoryAllocation = 0,
        .DmaAddress = Nothing
    })

        Devices.Add(New Device With {
        .Name = "SerialPort",
        .InputPorts = New List(Of UShort) From {&H3F8, &H3FB},
        .OutputPorts = New List(Of UShort) From {&H3F8, &H3FC},
        .MemoryAllocation = 0,
        .DmaAddress = Nothing
    })

        Devices.Add(New Device With {
        .Name = "SoundCard",
        .InputPorts = New List(Of UShort)(),
        .OutputPorts = New List(Of UShort) From {&H220, &H221, &H222},
        .MemoryAllocation = 0,
        .DmaAddress = Nothing
    })

        Devices.Add(New Device With {
        .Name = "NetworkCard",
        .InputPorts = New List(Of UShort) From {&H300, &H301},
        .OutputPorts = New List(Of UShort) From {&H300, &H301, &H302},
        .MemoryAllocation = networkMemSize,
        .DmaAddress = networkAddress
    })

        Devices.Add(New Device With {
    .Name = "RTC",
    .InputPorts = New List(Of UShort) From {&H70, &H71, &H72, &H73, &H74, &H75, &H76, &H77},
    .OutputPorts = New List(Of UShort)(),
    .MemoryAllocation = 0,
    .DmaAddress = Nothing
})
        Devices.Add(New Device With {
            .Name = "RealFileSystem",
            .InputPorts = New List(Of UShort) From {&H400, &H401, &H402, &H403},
            .OutputPorts = New List(Of UShort) From {&H400, &H401, &H402, &H403},
            .MemoryAllocation = 0,
            .DmaAddress = Nothing
        })

        Devices.Add(New Device With {
            .Name = "VirtualClipboard",
            .InputPorts = New List(Of UShort) From {&H500, &H501, &H502},
            .OutputPorts = New List(Of UShort) From {&H500, &H501, &H502},
            .MemoryAllocation = 0,
            .DmaAddress = Nothing
        })
        Devices.Add(New Device With {
            .Name = "VirtualSystemInfo",
            .InputPorts = New List(Of UShort) From {&H600, &H601, &H602},
            .OutputPorts = New List(Of UShort) From {&H600},
            .MemoryAllocation = 0,
            .DmaAddress = Nothing
        })
        Devices.Add(New Device With {
    .Name = "VirtualEmailManager",
    .InputPorts = New List(Of UShort) From {&H700, &H701, &H702},
    .OutputPorts = New List(Of UShort) From {&H700, &H701},
    .MemoryAllocation = 0,
    .DmaAddress = Nothing
})

        ' Verifichiamo che l'allocazione totale non superi la memoria disponibile
        Dim totalAllocated As UInteger = Devices.Sum(Function(d) d.MemoryAllocation)
        If totalAllocated > memorySize Then
            LogError("Errore: Allocazione di memoria totale supera la memoria disponibile")
        End If

        ' Imposta gli indirizzi DMA per i dispositivi che li utilizzano
    End Sub

    ' Metodo per aggiungere un dispositivo extra
    Public Sub AddExtraDevice(name As String, inputPorts As List(Of UShort), outputPorts As List(Of UShort))
        Dim newDevice As New Device With {
            .Name = name,
            .InputPorts = inputPorts,
            .OutputPorts = outputPorts,
            .MemoryAllocation = 0,
            .DmaAddress = Nothing
        }
        ExtraDevices.Add(newDevice)
        Devices.Add(newDevice)
    End Sub

    ' Metodo per salvare i dispositivi extra in un file di testo
    Public Sub SaveExtraDevicesToFile(filePath As String)
        Try
            Using writer As New StreamWriter(filePath)
                For Each device In ExtraDevices
                    Dim inputPorts = String.Join(";", device.InputPorts)
                    Dim outputPorts = String.Join(";", device.OutputPorts)
                    writer.WriteLine($"{device.Name},{inputPorts},{outputPorts}")
                Next
            End Using
        Catch ex As Exception
            LogError($"Errore nel salvataggio dei dispositivi extra: {ex.Message}")
        End Try
    End Sub

    ' Metodo per caricare i dispositivi extra da un file di testo
    Public Sub LoadExtraDevicesFromFile(filePath As String)
        Try
            If File.Exists(filePath) Then
                ExtraDevices.Clear()
                Using reader As New StreamReader(filePath)
                    While Not reader.EndOfStream
                        Dim line = reader.ReadLine()
                        Dim parts = line.Split(","c)
                        If parts.Length = 3 Then
                            Dim name = parts(0)
                            Dim inputPorts = parts(1).Split(";"c).Select(Function(p) CUShort(p)).ToList()
                            Dim outputPorts = parts(2).Split(";"c).Select(Function(p) CUShort(p)).ToList()
                            AddExtraDevice(name, inputPorts, outputPorts)
                        End If
                    End While
                End Using
            Else
                LogError("Errore: File non trovato.")
            End If
        Catch ex As Exception
            LogError($"Errore nel caricamento dei dispositivi extra: {ex.Message}")
        End Try
    End Sub

    Public Sub UpdateScreen(sender As Object, e As EventArgs)
        cpu.WaitBus = True

        ' Trova il dispositivo VGA nella lista dei dispositivi
        Dim vgaDevice = Devices.Find(Function(d) d.Name = "VGA")

        If vgaDevice.Name IsNot Nothing Then
            ' Usa l'allocazione di memoria e l'indirizzo DMA del dispositivo VGA
            Dim vramSize As UInteger = vgaDevice.MemoryAllocation
            Dim vramAddress As UInteger = vgaDevice.DmaAddress.GetValueOrDefault()

            ' Preleva i dati della VRAM dalla memoria
            Dim vramData(vramSize - 1) As Byte
            mem.LoadByteArray(vramData, vramAddress)

            ' Aggiorna il VirtualMonitor con i dati prelevati
            VGA.WriteVram(vramData)

            ' Disegna lo schermo virtuale
            VGA.DrawVirtualScreen()
        Else
            ' Gestione dell'errore se il dispositivo VGA non viene trovato
            LogError("Errore: Dispositivo VGA non trovato")
        End If

        cpu.WaitBus = False
    End Sub

    Public Sub ExecuteCPUCycle(sender As Object, e As EventArgs)
        cpu.ExecuteInstruction()
    End Sub

    Public Sub OnKeyDown(sender As Object, e As KeyEventArgs)
        ' Converti il codice tasto in uno scancode (questa è una semplificazione)
        Dim scanCode As Byte = CByte(e.KeyValue)
        Mouse_Key.OnKeyPress(scanCode)
        cpu.IrqAsk()
    End Sub

    Public Sub OnMouseMove(sender As Object, e As MouseEventArgs)
        Dim deltaX As Integer = CInt(e.X - lastMouseX)
        Dim deltaY As Integer = CInt(e.Y - lastMouseY)
        Mouse_Key.OnMouseMove(deltaX, deltaY)
        lastMouseX = e.X
        lastMouseY = e.Y
        cpu.IrqAsk()
    End Sub

    Public Sub OnMouseDown(sender As Object, e As MouseEventArgs)
        Dim button As Byte = GetMouseButton(e.Button)
        Mouse_Key.OnMouseButton(button, True)
        cpu.IrqAsk()
    End Sub

    Public Sub OnMouseUp(sender As Object, e As MouseEventArgs)
        Dim button As Byte = GetMouseButton(e.Button)
        Mouse_Key.OnMouseButton(button, False)
        cpu.IrqAsk()
    End Sub

    Public Function GetMouseButton(button As MouseButtons) As Byte
        Select Case button
            Case MouseButtons.Left
                Return &H1
            Case MouseButtons.Right
                Return &H2
            Case MouseButtons.Middle
                Return &H4
            Case Else
                Return 0
        End Select
    End Function

    Private Sub cpu_Halt(sender As Object) Handles cpu.Halt
        ' Gestisce l'evento di halt della CPU
        RaiseEvent End_Execute(Me)
    End Sub

    Private Sub cpu_IngRQ(sender As Object, Port As UShort, size As Byte) Handles cpu.IngRQ
        ' Trova il dispositivo che gestisce questa porta di input
        Dim device = Devices.Find(Function(d) d.InputPorts.Contains(Port))
        If device.Name IsNot Nothing Then
            ' Gestione della richiesta di input per il dispositivo specifico
            Select Case device.Name
                Case "HardDisk"
                    HandleHardDiskInput(Port, size)
                Case "Keyboard"
                    HandleKeyboardInput(Port, size)
                Case "Mouse"
                    HandleMouseInput(Port, size)
                Case "SerialPort"
                    HandleSerialPortInput(Port, size)
                Case "NetworkCard"
                    HandleNetworkCardInput(Port, size)
                Case "RTC"
                    HandleRTCInput(Port, size)
                Case device.Name.StartsWith("Extra_")
                    HandleExtraDeviceInput(device, Port, size)
                Case "RealFileSystem"
                    HandleRealFileSystemInput(Port, size)
                Case "VirtualClipboard"
                    HandleVirtualClipboardInput(Port, size)
                Case "VirtualSystemInfo"
                    HandleVirtualSystemInfoInput(Port, size)
                Case "virtualEmailManager"
                    HandleVirtualEmailManagerInput(Port, size)
            End Select
        Else
            LogError($"Nessun dispositivo trovato per la porta di input: {Port}")
        End If
    End Sub

    Private Sub cpu_OutRQ(sender As Object, port As UShort, size As Byte) Handles cpu.OutRQ
        ' Trova il dispositivo che gestisce questa porta di output
        Dim device = Devices.Find(Function(d) d.OutputPorts.Contains(port))
        If device.Name IsNot Nothing Then
            ' Gestione della richiesta di output per il dispositivo specifico
            Select Case device.Name
                Case "VGA"
                    HandleVGAOutput(port, size)
                Case "HardDisk"
                    HandleHardDiskOutput(port, size)
                Case "SerialPort"
                    HandleSerialPortOutput(port, size)
                Case "SoundCard"
                    HandleSoundCardOutput(port, size)
                Case "NetworkCard"
                    HandleNetworkCardOutput(port, size)
                Case device.Name.StartsWith("Extra_")
                    HandleExtraDeviceOutput(device, port, size)
                Case "RealFileSystem"
                    HandleRealFileSystemOutput(port, size)
                Case "VirtualClipboard"
                    HandleVirtualClipboardOutput(port, size)
                Case "VirtualSystemInfo"
                    HandleVirtualSystemInfoOutput(port, size)
                Case "virtualEmailManager"
                    HandleVirtualEmailManagerOutput(port, size)
            End Select
        Else
            LogError($"Nessun dispositivo trovato per la porta di output: {port}")
        End If
    End Sub

    Private Sub HandleVirtualEmailManagerInput(port As UShort, size As Byte)
        cpu.ValoreI_O = virtualEmailManager.ReadPort(port)
    End Sub

    Private Sub HandleVirtualEmailManagerOutput(port As UShort, size As Byte)
        virtualEmailManager.WritePort(port, CByte(cpu.ValoreI_O))
    End Sub

    Private Sub HandleVirtualSystemInfoInput(port As UShort, size As Byte)
        cpu.ValoreI_O = virtualSystemInfo.ReadPort(port)
    End Sub

    Private Sub HandleVirtualSystemInfoOutput(port As UShort, size As Byte)
        virtualSystemInfo.WritePort(port, CByte(cpu.ValoreI_O))
    End Sub

    Private Sub HandleVirtualClipboardInput(port As UShort, size As Byte)
        cpu.ValoreI_O = virtualClipboard.ReadPort(port)
    End Sub

    Private Sub HandleVirtualClipboardOutput(port As UShort, size As Byte)
        virtualClipboard.WritePort(port, CByte(cpu.ValoreI_O))
    End Sub

    Private Sub HandleRealFileSystemInput(port As UShort, size As Byte)
        cpu.ValoreI_O = realFileSystem.ReadPort(port)
    End Sub

    Private Sub HandleRealFileSystemOutput(port As UShort, size As Byte)
        realFileSystem.WritePort(port, CByte(cpu.ValoreI_O))
    End Sub
    Private Sub HandleHardDiskInput(Port As UShort, size As Byte)
        ' Implementa la logica per gestire l'input del disco rigido
        ' Ad esempio, leggere lo stato del disco o i dati
        Select Case Port
            Case &H1F0
                ' Lettura OFFEST_POS
                cpu.ValoreI_O = VHD.Pos_Offset
            Case &H1F1
                'Leggi valore diretto dal dispositivo
                Select Case size
                    Case 1 ' Byte
                        cpu.ValoreI_O = VHD.ReadByte()
                    Case 2 ' Word (UShort)
                        cpu.ValoreI_O = VHD.ReadWord()

                    Case 4 ' DWord (UInteger)
                        cpu.ValoreI_O = VHD.ReadDWord()
                End Select
            Case &H1F2
                ' carica dati da dma
                cpu.WaitBus = True
                Dim val As Byte = 0
                ' Trova il dispositivo VGA nella lista dei dispositivi
                Dim HDDevice = Devices.Find(Function(d) d.Name = "HardDisk")

                If HDDevice.Name IsNot Nothing Then
                    ' Usa l'allocazione di memoria e l'indirizzo DMA del dispositivo VGA
                    Dim HDSize As UInteger = HDDevice.MemoryAllocation
                    Dim HDAddress As UInteger = HDDevice.DmaAddress.GetValueOrDefault()

                    ' Preleva i dati della hdRAM dalla memoria
                    Dim HDData() As Byte
                    VHD.LoadByteArray(HDData)
                    mem.WriteByteArray(HDData, HDAddress)

                    val = 255
                Else
                    ' Gestione dell'errore se il dispositivo hd non viene trovato
                    LogError("Errore: Dispositivo HD non trovato")
                End If
                cpu.ValoreI_O = val

                cpu.WaitBus = False
        End Select
    End Sub

    Private Sub HandleKeyboardInput(Port As UShort, size As Byte)
        Select Case size
            Case 1 ' Byte
                Dim byteValue As Byte = Mouse_Key.ReadKeyboardPort(Port)
                cpu.ValoreI_O = byteValue

            Case 2 ' Word (UShort)
                Dim lowByte As Byte = Mouse_Key.ReadKeyboardPort(Port)
                Dim highByte As Byte = Mouse_Key.ReadKeyboardPort(Port)
                Dim wordValue As UShort = CUShort((CUShort(highByte) << 8) Or lowByte)
                cpu.ValoreI_O = wordValue

            Case 4 ' DWord (UInteger)
                Dim value As UInteger = 0
                For i As Integer = 0 To 3
                    Dim byteValue As Byte = Mouse_Key.ReadKeyboardPort(Port)
                    value = value Or (CUInt(byteValue) << (8 * i))
                Next
                cpu.ValoreI_O = value

        End Select
    End Sub

    Private Sub HandleMouseInput(Port As UShort, size As Byte)
        Select Case size
            Case 1 ' Byte
                Dim byteValue As Byte = Mouse_Key.ReadMousePort(Port)
                cpu.ValoreI_O = byteValue

            Case 2 ' Word (UShort)
                Dim lowByte As Byte = Mouse_Key.ReadMousePort(Port)
                Dim highByte As Byte = Mouse_Key.ReadMousePort(Port)
                Dim wordValue As UShort = CUShort((CUShort(highByte) << 8) Or lowByte)
                cpu.ValoreI_O = wordValue

            Case 4 ' DWord (UInteger)
                Dim value As UInteger = 0
                For i As Integer = 0 To 3
                    Dim byteValue As Byte = Mouse_Key.ReadMousePort(Port)
                    value = value Or (CUInt(byteValue) << (8 * i))
                Next
                cpu.ValoreI_O = value
        End Select
    End Sub

    Private Sub HandleSerialPortInput(Port As UShort, size As Byte)
        ' Implementa la logica per gestire l'input della porta seriale
        Select Case Port
            Case &H3F8 ' PORT_DATA
                ' Lettura dei dati ricevuti
                Select Case size
                    Case 1 ' Byte
                        cpu.ValoreI_O = serialManager.ReadPort(&H3F8)
                    Case 2 ' Word (UShort)
                        Dim lowByte As Byte = serialManager.ReadPort(&H3F8)
                        Dim highByte As Byte = serialManager.ReadPort(&H3F8)
                        cpu.ValoreI_O = CUShort((CUShort(highByte) << 8) Or lowByte)
                    Case 4 ' DWord (UInteger)
                        Dim value As UInteger = 0
                        For i As Integer = 0 To 3
                            Dim byteValue As Byte = serialManager.ReadPort(&H3F8)
                            value = value Or (CUInt(byteValue) << (8 * i))
                        Next
                        cpu.ValoreI_O = value
                End Select
            Case &H3FD ' PORT_STATUS
                ' Lettura 
                cpu.ValoreI_O = serialManager.ReadPort(&H3FD)
            Case Else
                LogError($"Porta di input seriale non gestita: {Port}")
        End Select
    End Sub

    Private Sub HandleNetworkCardInput(Port As UShort, size As Byte)
        Select Case Port
            Case &H300 ' Porta di stato
                ' Leggi lo stato della scheda di rete
                Dim status As Byte = 0
                If networkManager.isRunning Then
                    status = status Or &H1 ' Bit 0: Scheda attiva
                End If
                If networkManager.ReadPort(NetworkManager.PORT_STATUS) > 0 Then
                    status = status Or &H2 ' Bit 1: Dati disponibili
                End If
                cpu.ValoreI_O = status
            Case &H301 ' Porta per leggere la quantità di dati disponibili
                Dim bytesAvailable As Integer = networkManager.BytesToRead()
                Select Case size
                    Case 1
                        cpu.ValoreI_O = CByte(bytesAvailable And &HFF)
                    Case 2
                        cpu.ValoreI_O = CUShort(bytesAvailable And &HFFFF)
                    Case 4
                        cpu.ValoreI_O = CUInt(bytesAvailable)
                End Select
            Case Else
                LogError($"Porta di input rete non gestita: {Port}")
        End Select
    End Sub

    ' Funzioni di gestione dell'output per ciascun dispositivo
    Private Sub HandleVGAOutput(Port As UShort, size As Byte)
        ' Implementa la logica per gestire l'output VGA
        ' Ad esempio, scrivere nei registri VGA o aggiornare lo schermo
        Select Case Port
            Case &H3C0
                Dim ByteVal As Byte
                Select Case size
                    Case 1
                        ByteVal = cpu.ValoreI_O
                    Case 2, 4
                        ByteVal = CByte(cpu.ValoreI_O And &HFF)
                End Select
                VGA.ResizeVideo(ByteVal)
            Case &H3C1
                Select Case cpu.ValoreI_O
                    Case &H0
                        VGA.ClearVram()
                    Case &HFF
                        cpu.WaitBus = True

                        ' Trova il dispositivo VGA nella lista dei dispositivi
                        Dim vgaDevice = Devices.Find(Function(d) d.Name = "VGA")

                        If vgaDevice.Name IsNot Nothing Then
                            ' Usa l'allocazione di memoria e l'indirizzo DMA del dispositivo VGA
                            Dim vramSize As UInteger = vgaDevice.MemoryAllocation
                            Dim vramAddress As UInteger = vgaDevice.DmaAddress.GetValueOrDefault()

                            ' Preleva i dati della VRAM dalla memoria
                            Dim vramData(vramSize - 1) As Byte
                            mem.LoadByteArray(vramData, vramAddress)

                            ' Aggiorna il VirtualMonitor con i dati prelevati
                            VGA.WriteVram(vramData)

                            ' Disegna lo schermo virtuale
                            VGA.DrawVirtualScreen()
                        Else
                            ' Gestione dell'errore se il dispositivo VGA non viene trovato
                            LogError("Errore: Dispositivo VGA non trovato")
                        End If

                        cpu.WaitBus = False
                End Select
            Case &H3C2
                VGA.Width = cpu.ValoreI_O
            Case &H3C3
                VGA.Width = cpu.ValoreI_O

        End Select
    End Sub

    Private Sub HandleHardDiskOutput(Port As UShort, size As Byte)
        ' Implementa la logica per gestire l'input del disco rigido
        ' Ad esempio, leggere lo stato del disco o i dati
        Select Case Port
            Case &H1F0
                ' scrivi sectorsize
                VHD.Pos_Offset = cpu.ValoreI_O
            Case &H1F1
                'scrivi valore diretto dal dispositivo
                Select Case size
                    Case 1 ' Byte
                        VHD.WriteByte(cpu.ValoreI_O)
                    Case 2 ' Word (UShort)
                        VHD.WriteWord(cpu.ValoreI_O)
                    Case 4 ' DWord (UInteger)
                        VHD.WriteDWord(cpu.ValoreI_O)
                End Select
            Case &H1F2
                Select Case cpu.ValoreI_O
                    Case &H0
                        ' carica dati du dma
                        cpu.WaitBus = True
                        Dim val As Byte = 0
                        ' Trova il dispositivo VGA nella lista dei dispositivi
                        Dim HDDevice = Devices.Find(Function(d) d.Name = "HardDisk")

                        If HDDevice.Name IsNot Nothing Then
                            ' Usa l'allocazione di memoria e l'indirizzo DMA del dispositivo VGA
                            Dim HDSize As UInteger = HDDevice.MemoryAllocation
                            Dim HDAddress As UInteger = HDDevice.DmaAddress.GetValueOrDefault()

                            ' Preleva i dati della VRAM dalla memoria
                            Dim hdData(HDSize - 1) As Byte
                            mem.LoadByteArray(hdData, HDAddress)
                            VHD.WriteByteArray(hdData)
                            ' Aggiorna il VirtualMonitor con i dati prelevati

                        Else
                            ' Gestione dell'errore se il dispositivo VGA non viene trovato
                            LogError("Errore: Dispositivo HD non trovato")
                        End If
                        cpu.WaitBus = False
                    Case &HFF
                        'ridimensiona il disco
                        VHD.ClearHD()
                    Case Else
                        'ridimensiona il disco
                        VHD.SetHDSize(cpu.ValoreI_O)
                End Select
        End Select
    End Sub

    Private Sub HandleSerialPortOutput(Port As UShort, size As Byte)
        ' Implementa la logica per gestire l'output della porta seriale
        Select Case Port
            Case &H3F8 ' PORT_DATA
                ' Invio di dati
                Select Case size
                    Case 1 ' Byte
                        serialManager.WritePort(CByte(cpu.ValoreI_O))
                    Case 2 ' Word (UShort)
                        serialManager.WritePort(CByte(cpu.ValoreI_O And &HFF))
                        serialManager.WritePort(CByte((cpu.ValoreI_O >> 8) And &HFF))
                    Case 4 ' DWord (UInteger)
                        serialManager.WritePort(CByte(cpu.ValoreI_O And &HFF))
                        serialManager.WritePort(CByte((cpu.ValoreI_O >> 8) And &HFF))
                        serialManager.WritePort(CByte((cpu.ValoreI_O >> 16) And &HFF))
                        serialManager.WritePort(CByte((cpu.ValoreI_O >> 24) And &HFF))
                End Select
            Case &H3FC ' PORT_DATA
                If cpu.ValoreI_O = &H0 Then
                    serialManager.Start()
                Else ' PORT_DATA
                    serialManager.Stop()
                End If
            Case Else
                LogError($"Porta di output seriale non gestita: {Port}")
        End Select
    End Sub

    Private Sub HandleSoundCardOutput(Port As UShort, size As Byte)
        Select Case Port
            Case &H220  ' Porte di output della Sound Blaster
                Select Case size
                    Case 1 ' Byte
                        soundCard.WritePort(CByte(cpu.ValoreI_O))
                    Case 2 ' Word (UShort)
                        soundCard.WritePort(CByte(cpu.ValoreI_O And &HFF))
                        soundCard.WritePort(CByte((cpu.ValoreI_O >> 8) And &HFF))
                    Case 4 ' DWord (UInteger)
                        soundCard.WritePort(CByte(cpu.ValoreI_O And &HFF))
                        soundCard.WritePort(CByte((cpu.ValoreI_O >> 8) And &HFF))
                        soundCard.WritePort(CByte((cpu.ValoreI_O >> 16) And &HFF))
                        soundCard.WritePort(CByte((cpu.ValoreI_O >> 24) And &HFF))
                End Select
            Case &H221
                If cpu.ValoreI_O = &H0 Then
                    soundCard.Start()
                Else ' PORT_CONTROL
                    soundCard.Stop()
                End If
            Case &H222 ' PORT_CONTROL
                soundCard.BEEP_DURATION = cpu.ValoreI_O
            Case Else
                LogError($"Porta di output Sound Blaster non gestita: {Port}")
        End Select
    End Sub

    Private Sub HandleNetworkCardOutput(Port As UShort, size As Byte)
        Select Case Port
            Case &H300 ' Porta di controllo
                Select Case cpu.ValoreI_O
                    Case 0 ' Comando: Inizia ricezione
                        StartNetworkReceive()
                    Case 1 ' Comando: Inizia trasmissione
                        StartNetworkTransmit()
                    Case 4 ' Comando: Avvia il NetworkManager
                        networkManager.Start()
                    Case 5 ' Comando: Ferma il NetworkManager
                        networkManager.Stop()
                    Case 6
                        networkManager.InitializeUdpClient()
                    Case Else
                        LogError($"Comando di rete non riconosciuto: {cpu.ValoreI_O}")
                End Select
            Case &H301 ' Porta per impostare indirizzo ip
                networkManager._IPAddress = cpu.ValoreI_O
            Case &H302 ' Porta per impostare port
                networkManager._Port = cpu.ValoreI_O
            Case Else
                LogError($"Porta di output rete non gestita: {Port}")
        End Select
    End Sub

    Private Sub StartNetworkReceive()
        ' Trova il dispositivo NetworkCard nella lista dei dispositivi
        Dim networkDevice = Devices.Find(Function(d) d.Name = "NetworkCard")
        If networkDevice.Name IsNot Nothing AndAlso networkDevice.DmaAddress.HasValue Then
            Dim networkAddress As UInteger = networkDevice.DmaAddress.Value
            Dim networkSize As UInteger = networkDevice.MemoryAllocation

            ' Prepara un buffer per i dati ricevuti
            Dim receivedData As New List(Of Byte)()
            Dim bytesRead As Integer = 0
            Dim eodMarker As Byte() = System.Text.Encoding.ASCII.GetBytes("#EOD")

            ' Leggi i dati dalla rete
            While bytesRead < networkSize - eodMarker.Length ' Lascia spazio per #EOD
                Dim data As Byte = networkManager.ReadPort(NetworkManager.PORT_DATA)
                If data > 0 Then
                    receivedData.Add(data)
                    bytesRead += 1
                Else
                    Exit While ' Nessun altro dato disponibile
                End If
            End While

            ' Aggiungi il marcatore #EOD
            receivedData.AddRange(eodMarker)

            ' Prepara l'array finale da scrivere in memoria
            Dim finalData(receivedData.Count - 1) As Byte
            receivedData.CopyTo(finalData)

            cpu.WaitBus = True
            ' Scrivi i dati ricevuti nella memoria DMA
            mem.WriteByteArray(finalData, networkAddress)
            cpu.WaitBus = False
        Else
            LogError("Errore: Dispositivo NetworkCard non trovato o indirizzo DMA non valido")
        End If
    End Sub

    Private Sub StartNetworkTransmit()
        ' Trova il dispositivo NetworkCard nella lista dei dispositivi
        Dim networkDevice = Devices.Find(Function(d) d.Name = "NetworkCard")
        If networkDevice.Name IsNot Nothing AndAlso networkDevice.DmaAddress.HasValue Then
            Dim networkAddress As UInteger = networkDevice.DmaAddress.Value
            Dim networkSize As UInteger = networkDevice.MemoryAllocation
            cpu.WaitBus = True
            ' Leggi i dati dalla memoria DMA
            Dim dataToSend(networkSize - 1) As Byte
            mem.LoadByteArray(dataToSend, networkAddress)
            cpu.WaitBus = False

            ' Cerca la sequenza "#EOD" per determinare la fine effettiva dei dati
            Dim eodMarker As Byte() = System.Text.Encoding.ASCII.GetBytes("#EOD")
            Dim effectiveLength As Integer = FindSequence(dataToSend, eodMarker)

            If effectiveLength = -1 Then
                effectiveLength = dataToSend.Length
            Else
                ' Rimuovi il marcatore "#EOD" dai dati da inviare
                effectiveLength -= eodMarker.Length
            End If

            ' Ridimensiona l'array per contenere solo i dati effettivi
            ReDim Preserve dataToSend(effectiveLength - 1)

            ' Invia i dati
            networkManager.BroadcastData(dataToSend)

        Else
            LogError("Errore: Dispositivo NetworkCard non trovato o indirizzo DMA non valido")
        End If
    End Sub

    Private Sub HandleRTCInput(Port As UShort, size As Byte)
        Try
            cpu.ValoreI_O = ReadRTCRegister(Port)
        Catch ex As ArgumentException
            LogError($"Errore nella lettura del registro RTC: {ex.Message}")
        End Try
    End Sub

    Private Sub HandleExtraDeviceInput(device As Device, Port As UShort, size As Byte)
        Try
            Dim inputFunc = GetInputFunction(device.Name)
            If inputFunc IsNot Nothing Then
                cpu.ValoreI_O = inputFunc(Port, size)
            Else
                LogError($"Funzione di input non trovata per {device.Name}")
            End If
        Catch ex As Exception
            LogError($"Error handling input per dispositivo {device.Name}: {ex.Message}")
            cpu.ValoreI_O = 0
        End Try
    End Sub

    Private Sub HandleExtraDeviceOutput(device As Device, Port As UShort, size As Byte)
        Try
            Dim outputFunc = GetOutputFunction(device.Name)
            If outputFunc IsNot Nothing Then
                outputFunc(Port, size, cpu.ValoreI_O)
            Else
                LogError($"Funzione di output non trovata per {device.Name}")
            End If
        Catch ex As Exception
            LogError($"Error handling output per dispositivo {device.Name}: {ex.Message}")
        End Try
    End Sub

    Private Function GetInputFunction(deviceName As String) As InputFunction
        If Not inputFunctions.ContainsKey(deviceName) Then
            Dim dllHandle = LoadDeviceDll(deviceName)
            Dim funcPtr = GetProcAddress(dllHandle, "Input")
            If funcPtr <> IntPtr.Zero Then
                inputFunctions(deviceName) = CType(Marshal.GetDelegateForFunctionPointer(funcPtr, GetType(InputFunction)), InputFunction)
            Else
                LogError($"Funzione di input non trovata in {deviceName}.dll")
            End If
        End If
        Return inputFunctions(deviceName)
    End Function

    Private Function GetOutputFunction(deviceName As String) As OutputFunction
        If Not outputFunctions.ContainsKey(deviceName) Then
            Dim dllHandle = LoadDeviceDll(deviceName)
            Dim funcPtr = GetProcAddress(dllHandle, "Output")
            If funcPtr <> IntPtr.Zero Then
                outputFunctions(deviceName) = CType(Marshal.GetDelegateForFunctionPointer(funcPtr, GetType(OutputFunction)), OutputFunction)
            Else
                LogError($"Funzione di output non trovata in {deviceName}.dll")
            End If
        End If
        Return outputFunctions(deviceName)
    End Function

    Private Function LoadDeviceDll(deviceName As String) As IntPtr
        If Not loadedDlls.ContainsKey(deviceName) Then
            Dim dllPath = $"{deviceName}.dll"
            Dim dllHandle = LoadLibrary(dllPath)
            If dllHandle <> IntPtr.Zero Then
                loadedDlls(deviceName) = dllHandle
            Else
                LogError($"Caricamento Fallito per {dllPath}")
            End If
        End If
        Return loadedDlls(deviceName)
    End Function

    Protected Overrides Sub Finalize()
        For Each dllHandle In loadedDlls.Values
            FreeLibrary(dllHandle)
        Next
        MyBase.Finalize()
    End Sub

End Class
