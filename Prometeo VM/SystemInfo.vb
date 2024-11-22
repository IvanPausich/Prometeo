Imports System.Net
Imports System.Net.Sockets

Public Class SystemInfo
    Public Shared Function GetIndirizzoIP() As String
        Try
            Dim host As IPHostEntry = Dns.GetHostEntry(Dns.GetHostName())
            For Each ip As IPAddress In host.AddressList
                If ip.AddressFamily = AddressFamily.InterNetwork Then
                    Return ip.ToString()
                End If
            Next
            Return "127.0.0.1"  ' Fallback to localhost if no IPv4 address found
        Catch ex As Exception
            Return "Error: " & ex.Message
        End Try
    End Function

    Public Shared Function GetNomeUtente() As String
        Try
            Return Environment.UserName
        Catch ex As Exception
            Return "Error: " & ex.Message
        End Try
    End Function
End Class