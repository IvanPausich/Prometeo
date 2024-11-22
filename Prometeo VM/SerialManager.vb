Imports System.Runtime.InteropServices
Imports System.Threading

Public Class USBSerialManager
    Private handle As IntPtr = IntPtr.Zero
    Private isRunning As Boolean = False
    Private receiveThread As Thread
    Private rxBuffer As New Queue(Of Byte)
    Private rxBufferLock As New Object()

    ' Classe per gli argomenti dell'evento DataReceived

    ' Costanti per le porte I/O simulate
    Private Const PORT_DATA As UShort = &H3F8
    Private Const PORT_STATUS As UShort = &H3FD
    Private Const PORT_CONTROL As UShort = &H3FB

    ' Win32 API Constants
    Private Const GENERIC_READ As UInteger = &H80000000UI
    Private Const GENERIC_WRITE As UInteger = &H40000000UI
    Private Const OPEN_EXISTING As UInteger = 3
    Private Const FILE_ATTRIBUTE_NORMAL As UInteger = &H80UI
    Private INVALID_HANDLE_VALUE As IntPtr = New IntPtr(-1)

    ' Win32 API Structures
    <StructLayout(LayoutKind.Sequential)>
    Private Structure DCB
        Public DCBlength As Integer
        Public BaudRate As Integer
        Public Flags As UInteger
        Public wReserved As UShort
        Public XonLim As UShort
        Public XoffLim As UShort
        Public ByteSize As Byte
        Public Parity As Byte
        Public StopBits As Byte
        Public XonChar As Char
        Public XoffChar As Char
        Public ErrorChar As Char
        Public EofChar As Char
        Public EvtChar As Char
        Public wReserved1 As UShort
    End Structure

    <StructLayout(LayoutKind.Sequential)>
    Private Structure COMMTIMEOUTS
        Public ReadIntervalTimeout As UInteger
        Public ReadTotalTimeoutMultiplier As UInteger
        Public ReadTotalTimeoutConstant As UInteger
        Public WriteTotalTimeoutMultiplier As UInteger
        Public WriteTotalTimeoutConstant As UInteger
    End Structure

    ' Win32 API Functions
    <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Private Shared Function CreateFile(
        lpFileName As String,
        dwDesiredAccess As UInteger,
        dwShareMode As UInteger,
        lpSecurityAttributes As IntPtr,
        dwCreationDisposition As UInteger,
        dwFlagsAndAttributes As UInteger,
        hTemplateFile As IntPtr) As IntPtr
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function CloseHandle(hObject As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function ReadFile(
        hFile As IntPtr,
        lpBuffer As Byte(),
        nNumberOfBytesToRead As UInteger,
        ByRef lpNumberOfBytesRead As UInteger,
        lpOverlapped As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function WriteFile(
        hFile As IntPtr,
        lpBuffer As Byte(),
        nNumberOfBytesToWrite As UInteger,
        ByRef lpNumberOfBytesWritten As UInteger,
        lpOverlapped As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function GetCommState(hFile As IntPtr, ByRef lpDCB As DCB) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function SetCommState(hFile As IntPtr, ByRef lpDCB As DCB) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function SetCommTimeouts(hFile As IntPtr, ByRef lpCommTimeouts As COMMTIMEOUTS) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    Public Sub New()
        handle = CreateFile($"\\.\COM1", GENERIC_READ Or GENERIC_WRITE, 0, IntPtr.Zero, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, IntPtr.Zero)
        If handle = INVALID_HANDLE_VALUE Then
            LogError($"Failed to open port COM1")
        End If

        Dim dcb As New DCB()
        If Not GetCommState(handle, dcb) Then
            LogError("Failed to get COM state")
        End If

        dcb.BaudRate = 9600
        dcb.ByteSize = 8
        dcb.Parity = 0 ' None
        dcb.StopBits = 0 ' 1 stop bit

        If Not SetCommState(handle, dcb) Then
            LogError("Failed to set COM state")
        End If

        Dim timeouts As New COMMTIMEOUTS()
        timeouts.ReadIntervalTimeout = 50
        timeouts.ReadTotalTimeoutConstant = 50
        timeouts.ReadTotalTimeoutMultiplier = 10
        timeouts.WriteTotalTimeoutConstant = 50
        timeouts.WriteTotalTimeoutMultiplier = 10

        If Not SetCommTimeouts(handle, timeouts) Then
            LogError("Failed to set COM timeouts")
        End If
    End Sub

    Public Sub Start()
        isRunning = True
        receiveThread = New Thread(AddressOf ReceiveLoop)
        receiveThread.Start()
    End Sub

    Public Sub [Stop]()
        isRunning = False
        If receiveThread IsNot Nothing Then
            receiveThread.Join()
        End If
        If handle <> IntPtr.Zero Then
            CloseHandle(handle)
            handle = IntPtr.Zero
        End If
    End Sub

    Private Sub ReceiveLoop()
        Dim buffer(255) As Byte
        Dim bytesRead As UInteger
        While isRunning
            If ReadFile(handle, buffer, CUInt(buffer.Length), bytesRead, IntPtr.Zero) AndAlso bytesRead > 0 Then
                Dim receivedData(CInt(bytesRead) - 1) As Byte
                Array.Copy(buffer, receivedData, bytesRead)

                ' Genera l'evento DataReceived

                SyncLock rxBufferLock
                    For i As Integer = 0 To bytesRead - 1
                        rxBuffer.Enqueue(buffer(i))
                    Next
                End SyncLock
            End If
            Thread.Sleep(1) ' Prevent tight loop
        End While
    End Sub

    Public Function ReadPort(port As UShort) As Byte
        Select Case port
            Case PORT_DATA
                SyncLock rxBufferLock
                    If rxBuffer.Count > 0 Then
                        Return rxBuffer.Dequeue()
                    Else
                        Return 0
                    End If
                End SyncLock
            Case PORT_STATUS
                Dim status As Byte = &H20 ' Transmit buffer always ready
                SyncLock rxBufferLock
                    If rxBuffer.Count > 0 Then
                        status = status Or &H1
                    End If
                End SyncLock
                Return status
            Case Else
                logerror("Invalid port for read operation")
        End Select
    End Function

    Public Sub WritePort(value As Byte)
        Dim buffer(0) As Byte
        buffer(0) = value
        Dim bytesWritten As UInteger
        WriteFile(handle, buffer, 1, bytesWritten, IntPtr.Zero)
    End Sub

End Class