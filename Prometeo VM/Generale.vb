Imports System.Net
Imports System.Runtime.InteropServices

Module Generale
    Private errors As New List(Of String)

    <DllImport("kernel32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
    Public Function LoadLibrary(lpFileName As String) As IntPtr
    End Function

    <DllImport("kernel32.dll", CharSet:=CharSet.Ansi, SetLastError:=True)>
    Public Function GetProcAddress(hModule As IntPtr, procName As String) As IntPtr
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Public Function FreeLibrary(hModule As IntPtr) As Boolean
    End Function

    Public Function App_Path() As String
        Return AppDomain.CurrentDomain.BaseDirectory
    End Function

    ' Funzione per limitare un valore entro un intervallo specifico
    Public Function Clamp(value As UInteger, minValue As UInteger, maxValue As UInteger) As UInteger
        If value < minValue Then
            Return minValue
        ElseIf value > maxValue Then
            Return maxValue
        Else
            Return value
        End If
    End Function

    ' Versione generica della funzione Clamp
    Public Function ClampGeneric(Of T As IComparable(Of T))(value As T, minValue As T, maxValue As T) As T
        If value.CompareTo(minValue) < 0 Then
            Return minValue
        ElseIf value.CompareTo(maxValue) > 0 Then
            Return maxValue
        Else
            Return value
        End If
    End Function

    ' Funzione di supporto per trovare una sequenza di byte in un array
    Public Function FindSequence(data As Byte(), sequence As Byte()) As Integer
        For i As Integer = 0 To data.Length - sequence.Length
            Dim found As Boolean = True
            For j As Integer = 0 To sequence.Length - 1
                If data(i + j) <> sequence(j) Then
                    found = False
                    Exit For
                End If
            Next
            If found Then
                Return i
            End If
        Next
        Return -1
    End Function

    ' Funzioni di supporto per la conversione IP-UInteger
    Public Function IPToUInteger(ipAddress As IPAddress) As UInteger
        Dim bytes As Byte() = ipAddress.GetAddressBytes()
        Array.Reverse(bytes)
        Return BitConverter.ToUInt32(bytes, 0)
    End Function

    Public Function UIntegerToIP(ipUInt As UInteger) As IPAddress
        Dim bytes As Byte() = BitConverter.GetBytes(ipUInt)
        Array.Reverse(bytes)
        Return New IPAddress(bytes)
    End Function

    ' Costanti per gli indirizzi dei registri
    Public Const SECONDS_REG As UShort = &H70
    Public Const MINUTES_REG As UShort = &H71
    Public Const HOURS_REG As UShort = &H72
    Public Const DAY_REG As UShort = &H73
    Public Const MONTH_REG As UShort = &H74
    Public Const YEAR_LOW_REG As UShort = &H75
    Public Const YEAR_HIGH_REG As UShort = &H76
    Public Const CONTROL_REG As UShort = &H77

    Public Function ReadRTCRegister(address As UShort) As Byte
        Dim currentTime As DateTime = DateTime.Now
        Select Case address
            Case SECONDS_REG
                Return CByte(currentTime.Second)
            Case MINUTES_REG
                Return CByte(currentTime.Minute)
            Case HOURS_REG
                Return CByte(currentTime.Hour)
            Case DAY_REG
                Return CByte(currentTime.Day)
            Case MONTH_REG
                Return CByte(currentTime.Month)
            Case YEAR_LOW_REG
                Return CByte(currentTime.Year And &HFF)
            Case YEAR_HIGH_REG
                Return CByte((currentTime.Year >> 8) And &HFF)
            Case Else
                Return 0
        End Select
    End Function

    ' Metodo per aggiungere un nuovo errore
    Public Sub LogError(message As String)
        errors.Add(message)
    End Sub

End Module
