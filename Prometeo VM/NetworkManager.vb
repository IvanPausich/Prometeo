Imports System.Net
Imports System.Net.Sockets
Imports System.Threading

Public Class NetworkManager
    Private udpClient As UdpClient
    Private localEndPoint As IPEndPoint
    Private receiveThread As Thread
    Public isRunning As Boolean = False
    Private receiveBuffer As New Queue(Of Byte)
    Private sendBuffer As New Queue(Of Byte)
    Private bufferLock As New Object()

    ' Costanti per le porte I/O simulate
    Public Const PORT_DATA As UShort = &H300
    Public Const PORT_STATUS As UShort = &H301
    Public Const PORT_CONTROL As UShort = &H302

    Public Property _IPAddress As UInteger
    Public Property _Port As UInteger


    Public Sub InitializeUdpClient()
        localEndPoint = New IPEndPoint(UIntegerToIP(_IPAddress), _Port)
        If isRunning Then
            LogError("Impossibile cambiare porta mentre NetworkManager in esecuzione")
        End If
        Try
            udpClient = New UdpClient(localEndPoint)
        Catch ex As Exception
            LogError($"Errore inizializzazione UdpClient: {ex.Message}")
            Throw
        End Try
    End Sub

    Public Sub Start()
        SyncLock bufferLock
            If isRunning Then
                LogError("NetworkManager is already runningè già in esecuzione")
            End If

            If udpClient Is Nothing Then
                LogError("UdpClient non initializzata. Prima configura LocalPort.")
            End If

            isRunning = True
            receiveThread = New Thread(AddressOf ReceiveLoop)
            receiveThread.Start()
        End SyncLock
    End Sub

    Public Sub [Stop]()
        SyncLock bufferLock
            If Not isRunning Then
                Return
            End If

            isRunning = False
            receiveThread.Join()
            udpClient.Close()
            udpClient = Nothing
        End SyncLock
    End Sub

    Private Sub ReceiveLoop()
        While isRunning
            Try
                Dim receivedData As Byte() = udpClient.Receive(localEndPoint)

                SyncLock bufferLock
                    For Each b In receivedData
                        receiveBuffer.Enqueue(b)
                    Next
                End SyncLock

            Catch ex As Exception
                LogError($"Errore: {ex.Message}")
            End Try
        End While
    End Sub

    Public Sub SendData(data As Byte())
        Try
            udpClient.Send(data, data.Length, localEndPoint)
        Catch ex As Exception
            LogError($"Errore sending data: {ex.Message}")
        End Try
    End Sub

    Public Sub BroadcastData(data As Byte())
        SendData(data)
    End Sub

    Public Function ReadPort(port As UShort) As Byte
        Select Case port
            Case PORT_DATA
                SyncLock bufferLock
                    If receiveBuffer.Count > 0 Then
                        Return receiveBuffer.Dequeue()
                    Else
                        Return 0
                    End If
                End SyncLock
            Case PORT_STATUS
                Dim status As Byte = &H20 ' Transmit buffer always ready
                SyncLock bufferLock
                    If receiveBuffer.Count > 0 Then
                        status = status Or &H1 ' Data available to read
                    End If
                End SyncLock
                Return status
            Case Else
                LogError("Porta non valida per la ricezione dati")
        End Select
    End Function

    Public Sub WritePort(port As UShort, value As Byte)
        Select Case port
            Case PORT_DATA
                SyncLock bufferLock
                    sendBuffer.Enqueue(value)
                    If sendBuffer.Count >= 1024 Then ' Esempio: invia quando il buffer raggiunge 1KB
                        Dim data(1023) As Byte
                        For i As Integer = 0 To 1023
                            data(i) = sendBuffer.Dequeue()
                        Next
                        BroadcastData(data)
                    End If
                End SyncLock
            Case PORT_CONTROL
                ' Implementa la logica di controllo se necessario
            Case Else
                LogError("Porta non valida per l'invio dati")
        End Select
    End Sub

    ' Aggiungi questo nuovo metodo alla fine della classe
    Public Function BytesToRead() As Integer
        SyncLock bufferLock
            Return receiveBuffer.Count
        End SyncLock
    End Function
End Class