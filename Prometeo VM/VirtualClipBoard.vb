Public Class VirtualClipboard
    Private Const PORT_OPERATION As UShort = &H500
    Private Const PORT_DATA As UShort = &H501
    Private Const PORT_STATUS As UShort = &H502

    Private currentOperation As Byte = 0
    Private dataBuffer As New List(Of Byte)
    Private statusCode As Byte = 0

    ' Operazioni
    Private Const OP_COPY_TEXT As Byte = 1
    Private Const OP_PASTE_TEXT As Byte = 2
    Private Const OP_COPY_BINARY As Byte = 3
    Private Const OP_PASTE_BINARY As Byte = 4

    Public Function ReadPort(port As UShort) As Byte
        Select Case port
            Case PORT_STATUS
                Return statusCode
            Case PORT_DATA
                If dataBuffer.Count > 0 Then
                    Dim value = dataBuffer(0)
                    dataBuffer.RemoveAt(0)
                    Return value
                Else
                    Return 0
                End If
            Case Else
                Return 0
        End Select
    End Function

    Public Sub WritePort(port As UShort, value As Byte)
        Select Case port
            Case PORT_OPERATION
                ExecuteOperation(value)
            Case PORT_DATA
                dataBuffer.Add(value)
        End Select
    End Sub

    Private Sub ExecuteOperation(operation As Byte)
        currentOperation = operation
        statusCode = 0

        Select Case operation
            Case OP_COPY_TEXT
                Dim text = System.Text.Encoding.UTF8.GetString(dataBuffer.ToArray())
                ClipboardManager.CopiaTestoNellaClipboard(text)
                statusCode = 1 ' Success
                dataBuffer.Clear()

            Case OP_PASTE_TEXT
                Dim text = ClipboardManager.IncollaTestoDallaClipboard()
                dataBuffer.AddRange(System.Text.Encoding.UTF8.GetBytes(text))
                statusCode = 1 ' Success

            Case OP_COPY_BINARY
                ClipboardManager.CopiaDatiBinariNellaClipboard(dataBuffer.ToArray())
                statusCode = 1 ' Success
                dataBuffer.Clear()

            Case OP_PASTE_BINARY
                Dim binaryData = ClipboardManager.IncollaDatiBinariDallaClipboard()
                If binaryData IsNot Nothing Then
                    dataBuffer.AddRange(binaryData)
                    statusCode = 1 ' Success
                Else
                    statusCode = 2 ' Error
                End If

        End Select
    End Sub
End Class