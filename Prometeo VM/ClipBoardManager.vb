
Public Class ClipboardManager
    ' Copiare testo nella clipboard
    Public Shared Sub CopiaTestoNellaClipboard(testo As String)
        Clipboard.SetText(testo)
    End Sub

    ' Incollare testo dalla clipboard
    Public Shared Function IncollaTestoDallaClipboard() As String
        If Clipboard.ContainsText() Then
            Return Clipboard.GetText()
        Else
            Return String.Empty
        End If
    End Function

    ' Copiare dati binari nella clipboard
    Public Shared Sub CopiaDatiBinariNellaClipboard(dati As Byte())
        Dim dataObj As New DataObject()
        dataObj.SetData("BinaryData", dati)
        Clipboard.SetDataObject(dataObj)
    End Sub

    ' Incollare dati binari dalla clipboard
    Public Shared Function IncollaDatiBinariDallaClipboard() As Byte()
        If Clipboard.ContainsData("BinaryData") Then
            Return DirectCast(Clipboard.GetData("BinaryData"), Byte())
        Else
            Return Nothing
        End If
    End Function
End Class