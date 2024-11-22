Imports System.Threading

Public Class SoundCardManager
    Private Const BUFFER_SIZE As UInteger = 4096
    Private playbackBuffer As New Queue(Of Byte)(BUFFER_SIZE)
    Private bufferLock As New Object()
    Private isRunning As Boolean = False
    Private processingThread As Thread

    ' Costanti per la generazione del suono
    Private Const MIN_FREQUENCY As UInteger = 37
    Private Const MAX_FREQUENCY As UInteger = 32767
    Private _BEEP_DURATION As UInteger = 100  ' millisecondi

    Public WriteOnly Property BEEP_DURATION As UInteger  ' millisecondi
        Set(value As UInteger)
            _BEEP_DURATION = value
        End Set
    End Property

    Public Sub New()
        ' Costruttore vuoto
    End Sub

    Public Sub Start()
        If isRunning Then
            logerror("SoundCardManager is already running")
        End If

        isRunning = True
        processingThread = New Thread(AddressOf ProcessingLoop)
        processingThread.Start()
    End Sub

    Public Sub [Stop]()
        If Not isRunning Then
            Return
        End If

        isRunning = False
        processingThread.Join()
    End Sub

    Private Sub ProcessingLoop()
        While isRunning
            ' Riproduce i dati audio dal buffer di riproduzione
            SyncLock bufferLock
                If playbackBuffer.Count > 0 Then
                    Dim byteToPlay As Byte = playbackBuffer.Dequeue()
                    PlaySound(byteToPlay)
                End If
            End SyncLock

            Thread.Sleep(1) ' Previene un loop troppo stretto
        End While
    End Sub

    Private Sub PlaySound(value As Byte)
        ' Converte il byte in una frequenza udibile
        Dim frequency As Integer = CInt(MIN_FREQUENCY + (value / 255.0) * (MAX_FREQUENCY - MIN_FREQUENCY))
        Try
            Console.Beep(frequency, _BEEP_DURATION)
        Catch ex As Exception
            ' Gestisce eventuali errori nella generazione del suono
            logerror($"Errore nella generazione del suono: {ex.Message}")
        End Try
    End Sub

    Public Sub WritePort(value As Byte)
        SyncLock bufferLock
            If playbackBuffer.Count < BUFFER_SIZE Then
                playbackBuffer.Enqueue(value)
            End If
        End SyncLock
    End Sub

End Class