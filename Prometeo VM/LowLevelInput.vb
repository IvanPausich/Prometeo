Imports System.Collections.Concurrent

Public Class LowLevelInput
    ' Costanti per le porte I/O
    Private Const PORT_KB_DATA As UShort = &H60
    Private Const PORT_KB_STATUS As UShort = &H64

    ' Buffer per tastiera e mouse
    Private keyboardBuffer As New ConcurrentQueue(Of Byte)()
    Private mouseBuffer As New ConcurrentQueue(Of Integer)()

    ' Stato della tastiera
    Private keyboardStatus As Byte = &H10 ' Inizialmente vuoto e non in attesa di comandi

    ' Registri del controller
    Private commandByte As Byte = &H45 ' Valore predefinito
    Private outputPort As Byte = &HDF ' Valore predefinito

    ' Funzione per leggere da una porta
    Public Function ReadKeyboardPort(ByVal port As UShort) As Byte
        Select Case port
            Case PORT_KB_DATA
                Dim value As Byte
                If keyboardBuffer.TryDequeue(value) Then
                    keyboardStatus = keyboardStatus And Not &H1 ' Rimuovi il flag "output buffer full"
                    Return value
                Else
                    Return 0
                End If
            Case PORT_KB_STATUS
                Return keyboardStatus
            Case Else
                LogError($"Porta {port} non supportata")
        End Select
    End Function
    Public Function ReadMousePort(ByVal port As UShort) As Byte
        Select Case port
            Case PORT_KB_DATA
                Dim value As Byte
                If mouseBuffer.TryDequeue(value) Then
                    keyboardStatus = keyboardStatus And Not &H20 ' Rimuovi il flag "mouse output buffer full"
                    Return value
                Else
                    Return 0
                End If
            Case Else
                LogError($"Porta {port} non supportata")
        End Select
    End Function

    ' Funzione per scrivere su una porta
    Public Sub WriteKeyboardPort(ByVal port As UShort, ByVal value As Byte)
        Select Case port
            Case PORT_KB_DATA
                ' Implementa la logica per i comandi della tastiera
                HandleKeyboardCommand(value)
            Case PORT_KB_STATUS
                ' Implementa la logica per i comandi del controller
                HandleControllerCommand(value)
            Case Else
                LogError($"Porta {port} non supportata")
        End Select
    End Sub

    ' Gestisce i comandi inviati alla tastiera
    Private Sub HandleKeyboardCommand(ByVal command As Byte)
        ' Implementa la logica per i comandi della tastiera
        ' Ad esempio:
        Select Case command
            Case &HED ' Set/Reset LEDs
                keyboardBuffer.Enqueue(&HFA) ' ACK
            Case &HEE ' Echo
                keyboardBuffer.Enqueue(&HEE)
            Case Else
                ' Gestisci altri comandi...
        End Select
        keyboardStatus = keyboardStatus Or &H1 ' Setta il flag "output buffer full"
    End Sub

    ' Gestisce i comandi inviati al controller
    Private Sub HandleControllerCommand(ByVal command As Byte)
        ' Implementa la logica per i comandi del controller
        ' Ad esempio:
        Select Case command
            Case &H20 ' Read Command Byte
                keyboardBuffer.Enqueue(commandByte)
                keyboardStatus = keyboardStatus Or &H1 ' Setta il flag "output buffer full"
            Case &H60 ' Write Command Byte
                keyboardStatus = keyboardStatus Or &H2 ' Setta il flag "input buffer full"
            Case &HD0 ' Read Output Port
                keyboardBuffer.Enqueue(outputPort)
                keyboardStatus = keyboardStatus Or &H1 ' Setta il flag "output buffer full"
            Case &HD1 ' Write Output Port
                keyboardStatus = keyboardStatus Or &H2 ' Setta il flag "input buffer full"
            Case Else
                ' Gestisci altri comandi...
        End Select
    End Sub

    ' Metodo chiamato quando viene premuto un tasto
    Public Sub OnKeyPress(ByVal scanCode As Byte)
        keyboardBuffer.Enqueue(scanCode)
        keyboardStatus = keyboardStatus Or &H1 ' Setta il flag "output buffer full"
    End Sub

    ' Metodo chiamato quando il mouse si muove
    Public Sub OnMouseMove(ByVal deltaX As UInteger, ByVal deltaY As UInteger)
        mouseBuffer.Enqueue(&H8) ' Movimento del mouse

        ' Enqueue deltaX un byte alla volta
        mouseBuffer.Enqueue(CByte(deltaX And &HFF))
        mouseBuffer.Enqueue(CByte((deltaX >> 8) And &HFF))
        mouseBuffer.Enqueue(CByte((deltaX >> 16) And &HFF))
        mouseBuffer.Enqueue(CByte((deltaX >> 24) And &HFF))

        ' Enqueue deltaY un byte alla volta
        mouseBuffer.Enqueue(CByte(deltaY And &HFF))
        mouseBuffer.Enqueue(CByte((deltaY >> 8) And &HFF))
        mouseBuffer.Enqueue(CByte((deltaY >> 16) And &HFF))
        mouseBuffer.Enqueue(CByte((deltaY >> 24) And &HFF))

        keyboardStatus = keyboardStatus Or &H20 ' Setta il flag "mouse output buffer full"
    End Sub
    ' Metodo chiamato quando viene premuto un pulsante del mouse
    Public Sub OnMouseButton(ByVal button As Byte, ByVal isPressed As Boolean)
        Dim status As Byte = &H8 ' Base status
        If isPressed Then
            status = status Or button
        End If
        mouseBuffer.Enqueue(status)
        mouseBuffer.Enqueue(0) ' DeltaX
        mouseBuffer.Enqueue(0) ' DeltaY
        keyboardStatus = keyboardStatus Or &H20 ' Setta il flag "mouse output buffer full"
    End Sub
End Class