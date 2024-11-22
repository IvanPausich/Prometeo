Public Class VirtualSystemInfo
    Private Const PORT_OPERATION As UShort = &H600
    Private Const PORT_DATA As UShort = &H601
    Private Const PORT_STATUS As UShort = &H602

    Private currentOperation As Byte = 0
    Private dataBuffer As New List(Of Byte)
    Private statusCode As Byte = 0

    ' Operazioni
    Private Const OP_GET_IP As Byte = 1
    Private Const OP_GET_USERNAME As Byte = 2

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
        End Select
    End Sub

    Private Sub ExecuteOperation(operation As Byte)
        currentOperation = operation
        statusCode = 0
        dataBuffer.Clear()

        Select Case operation
            Case OP_GET_IP
                Dim ip = SystemInfo.GetIndirizzoIP()
                dataBuffer.AddRange(System.Text.Encoding.UTF8.GetBytes(ip))
                statusCode = 1 ' Success

            Case OP_GET_USERNAME
                Dim username = SystemInfo.GetNomeUtente()
                dataBuffer.AddRange(System.Text.Encoding.UTF8.GetBytes(username))
                statusCode = 1 ' Success

            Case Else
                statusCode = 2 ' Error
        End Select
    End Sub
End Class