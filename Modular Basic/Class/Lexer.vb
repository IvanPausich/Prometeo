Public Class Lexer
    Private _tokens As New List(Of List(Of VBParser.Parsed_Code))
    Private inFunzione As Boolean = False
    Private inCostruct As Boolean = False

    Public Sub Inizializza(code As String)
        Dim r As New List(Of VBParser.Parsed_Code)
        ' Inizializza il parser e compila
        Dim parser As New VBParser(code)
        Dim tokens = parser.Parse()

        For Each token In tokens
            Select Case token.Symbol_ID
                Case VBParser.Tok_Types.EOL, VBParser.Tok_Types.EOP
                    If r.Count > 0 Then
                        _tokens.Add(New List(Of VBParser.Parsed_Code)(r))
                        ' Crea una stringa con tutti i token della riga
                        Dim rigaCompleta = String.Join(" ", r.Select(Function(t) t.Token))
                        Console.WriteLine($"parole {r.Count}, riga: {_tokens.Count} - {rigaCompleta}")
                        r.Clear()
                    End If
                Case Else
                    r.Add(token)
            End Select
        Next
        Console.WriteLine($"righe {_tokens.Count}")
    End Sub

    Public Sub Lex()
        Dim errorMsg As String = ""
        ' Prima verifichiamo le parentesi
        If Not ControllaParentesi(errorMsg) Then
            Throw New Exception(errorMsg)
        End If

        Dim stackScope As New Stack(Of ScopeInfo)
        Dim rigaCorrente As Integer = 0

        For Each riga As List(Of VBParser.Parsed_Code) In _tokens
            rigaCorrente += 1
            ' Salta le righe vuote
            If riga.Count = 0 Then Continue For

            Try
                AnalizzaRiga(riga, stackScope, rigaCorrente)
            Catch ex As Exception
                Throw New Exception($"Errore alla riga {rigaCorrente}: {ex.Message}")
            End Try
        Next

        ' Alla fine, controlliamo se tutti gli scope sono stati chiusi
        If stackScope.Count > 0 Then
            Dim scopeNonChiuso = stackScope.Pop()
            Throw New Exception($"Blocco '{scopeNonChiuso.TipoBlocco}' aperto alla riga {scopeNonChiuso.Riga} non è stato chiuso")
        End If
    End Sub

    Private Sub AnalizzaRiga(riga As List(Of VBParser.Parsed_Code),
                        stackScope As Stack(Of ScopeInfo),
                        rigaCorrente As Integer)
        ' Ottieni il primo token significativo della riga
        Dim primoToken = riga(0).Token.Trim().ToLower()

        ' Verifica le dichiarazioni consentite fuori dai costrutti
        If Not inCostruct Then
            ' Se non è un commento, facciamo i controlli
            If riga(0).Symbol_ID <> VBParser.Tok_Types.COMMENT Then
                Select Case primoToken
                    Case "module", "class"
                ' Questi sono permessi direttamente
                    Case "public", "private"
                        ' Controllo che ci sia un token successivo
                        If riga.Count < 2 Then
                            Throw New Exception($"Dichiarazione incompleta dopo {primoToken}")
                        End If
                        Dim nextToken = riga(1).Token.ToLower()
                        If nextToken <> "class" AndAlso nextToken <> "module" Then
                            Throw New Exception($"'{primoToken} {nextToken}' non è consentito al di fuori di un modulo o una classe. Solo dichiarazioni di moduli e classi sono consentite a questo livello.")
                        End If
                    Case Else
                        Throw New Exception($"'{primoToken}' non è consentito al di fuori di un modulo o una classe. Solo dichiarazioni di moduli e classi sono consentite a questo livello.")
                End Select
            End If
        End If

        Select Case primoToken
        ' Blocchi di codice
            Case "if", "select", "for", "do", "while", "sub", "function", "class", "module"
                If primoToken = "sub" OrElse primoToken = "function" Then inFunzione = True
                If primoToken = "module" OrElse primoToken = "class" Then inCostruct = True
                stackScope.Push(New ScopeInfo(primoToken, rigaCorrente))
        ' Chiusure di blocchi
            Case "end"
                If riga.Count < 2 Then
                    Throw New Exception("Istruzione 'End' incompleta")
                End If
                Dim tipoChiusura = riga(1).Token.Trim().ToLower()
                ChiudiBlocco(stackScope, tipoChiusura, rigaCorrente)

            Case "next", "loop"
                ChiudiBlocco(stackScope, primoToken, rigaCorrente)

        ' Casi speciali
            Case "else", "elseif", "case"
                If stackScope.Count = 0 OrElse Not IsValidMiddleStatement(primoToken, stackScope.Peek().TipoBlocco) Then
                    Throw New Exception($"'{primoToken}' trovato fuori dal contesto appropriato")
                End If

        ' Dichiarazioni variabili e altre istruzioni
            Case "dim", "public", "private"
                ' Controlla la parola successiva
                Dim nextToken = riga(1).Token.ToLower()
                Select Case nextToken
                    Case "sub", "function", "class"
                        If nextToken = "sub" OrElse nextToken = "function" Then inFunzione = True
                        If nextToken = "class" Then inCostruct = True
                        stackScope.Push(New ScopeInfo(nextToken, rigaCorrente))
                    Case Else
                        ' Se non è una parola chiave speciale, deve essere una dichiarazione di variabile
                        ControllaValiditàDichiarazione(riga)
                End Select
        End Select
    End Sub

    Private Sub ChiudiBlocco(stackScope As Stack(Of ScopeInfo), tipoChiusura As String, rigaCorrente As Integer)
        If stackScope.Count = 0 Then
            Throw New Exception($"Chiusura '{tipoChiusura}' inattesa")
        End If

        Dim apertura = stackScope.Peek()
        Dim chiusuraValida = False
        Select Case tipoChiusura
            Case "if"
                chiusuraValida = (apertura.TipoBlocco = "if")
            Case "select"
                chiusuraValida = (apertura.TipoBlocco = "select")
            Case "sub"
                inFunzione = False
                chiusuraValida = (apertura.TipoBlocco = "sub")
            Case "function"
                inFunzione = False
                chiusuraValida = (apertura.TipoBlocco = "function")
            Case "class"
                inCostruct = False
                chiusuraValida = (apertura.TipoBlocco = "class")
            Case "module"
                inCostruct = False
                chiusuraValida = (apertura.TipoBlocco = "module")
            Case "next"
                chiusuraValida = (apertura.TipoBlocco = "for")
            Case "loop"
                chiusuraValida = (apertura.TipoBlocco = "do" OrElse apertura.TipoBlocco = "while")
        End Select

        If Not chiusuraValida Then
            Throw New Exception($"Chiusura '{tipoChiusura}' non corrisponde all'apertura '{apertura.TipoBlocco}' della riga {apertura.Riga}")
        End If

        stackScope.Pop()
    End Sub

    Private Function IsValidMiddleStatement(statement As String, currentScope As String) As Boolean
        Select Case statement
            Case "else", "elseif"
                Return currentScope = "if"
            Case "case"
                Return currentScope = "select"
            Case Else
                Return False
        End Select
    End Function

    Private Sub ControllaValiditàDichiarazione(riga As List(Of VBParser.Parsed_Code))
        Dim typeHandler As New VBTypesHandler()
        Dim i As Integer = 0


        ' Controllo del modificatore d'accesso
        Select Case riga(0).Token.ToLower()
            Case "dim"
            Case "private", "public"
                If inFunzione Then
                    Throw New Exception($"Modificatore {riga(0).Token} non consentito all'interno di una funzione/procedura")
                End If
            Case Else
                Throw New Exception($"Modificatore di dichiarazione non valido: {riga(0).Token}")
        End Select

        i += 1

        While i < riga.Count

            ' Nome variabile
            Dim nomeVar = riga(i).Token

            If Not Char.IsLetter(nomeVar(0)) AndAlso nomeVar(0) <> "_"c Then
                Throw New Exception($"Nome variabile non valido: {nomeVar}")
            End If

            i += 1

            ' Controlla se è un array
            Dim isArray As Boolean = False
            If i < riga.Count AndAlso riga(i).Token = "(" Then
                isArray = True
                i += 1 ' Salta la parentesi aperta

                ' Leggi le dimensioni dell'array
                While i < riga.Count AndAlso riga(i).Token <> ")"
                    ' Verifica che la dimensione sia un numero
                    If Not Integer.TryParse(riga(i).Token, Nothing) Then
                        Throw New Exception($"Dimensione array non valida: {riga(i).Token}")
                    End If

                    i += 1

                    ' Se c'è una virgola, significa che abbiamo altre dimensioni
                    If i < riga.Count AndAlso riga(i).Token = "," Then
                        i += 1 ' Salta la virgola
                        ' Verifica che ci sia un numero dopo la virgola
                        If i >= riga.Count OrElse Not Integer.TryParse(riga(i).Token, Nothing) Then
                            Throw New Exception("Manca la dimensione dopo la virgola nell'array")
                        End If
                        Continue While
                    End If
                End While

                If i >= riga.Count OrElse riga(i).Token <> ")" Then
                    Throw New Exception("Manca parentesi chiusa nella dichiarazione array")
                End If
                i += 1 ' Salta la parentesi chiusa
            End If

            ' Controllo se c'è As
            If i >= riga.Count OrElse riga(i).Token.ToLower() <> "as" Then
                Throw New Exception("Manca 'As' nella dichiarazione")
            End If

            i += 1

            ' Controllo tipo
            If i >= riga.Count Then
                Throw New Exception("Manca il tipo della variabile")
            End If

            Dim tipoVar = riga(i).Token

            Try
                typeHandler.GetTypeCode(tipoVar)
            Catch ex As ArgumentException
                Throw New Exception($"Tipo non valido: {tipoVar}")
            End Try

            i += 1

            ' Se c'è un'inizializzazione
            If i < riga.Count AndAlso riga(i).Token = "=" Then
                If isArray Then
                    Throw New Exception("Non è possibile inizializzare un array in una dichiarazione semplice")
                End If

                i += 1

                If i >= riga.Count Then
                    Throw New Exception("Manca il valore di inizializzazione dopo '='")
                End If

                Dim valore = riga(i).Token

                If typeHandler.IsNumericType(tipoVar) Then
                    Try
                        If Not typeHandler.ISInRange(tipoVar, valore) Then
                            Throw New Exception($"Valore {valore} fuori range per il tipo {tipoVar}")
                        End If
                    Catch ex As Exception
                        Throw New Exception($"Valore non valido per il tipo {tipoVar}: {valore}")
                    End Try
                End If

                i += 1
            End If

            ' Se c'è una virgola, continua con la prossima dichiarazione
            If i < riga.Count AndAlso riga(i).Token = "," Then
                i += 1
                Continue While
            End If
        End While
    End Sub


    Private Class ScopeInfo
        Public Property TipoBlocco As String
        Public Property Riga As Integer

        Public Sub New(tipo As String, r As Integer)
            TipoBlocco = tipo
            Riga = r
        End Sub
    End Class
    Public Function ControllaParentesi(ByRef errorMsg As String) As Boolean
        Dim stackParentesi As New Stack(Of ParentesiInfo)
        Dim rigaCorrente As Integer = 0

        For Each riga As List(Of VBParser.Parsed_Code) In _tokens
            rigaCorrente += 1
            Dim posizioneToken As Integer = 0

            For Each token As VBParser.Parsed_Code In riga
                posizioneToken += 1
                ' Controlliamo solo i token che contengono parentesi
                Dim c As String = token.Token.Trim() ' Rimosso PadRight non necessario

                Select Case c
                    Case "(", "[", "{"
                        stackParentesi.Push(New ParentesiInfo(c(0), rigaCorrente, posizioneToken))
                    Case ")", "]", "}"
                        If stackParentesi.Count = 0 Then
                            errorMsg = $"Parentesi di chiusura '{c}' non attesa alla riga {rigaCorrente}, posizione {posizioneToken}"
                            Return False
                        End If

                        Dim apertura As ParentesiInfo = stackParentesi.Pop()
                        If Not CorrispondonoParentesi(apertura.Carattere, c(0)) Then
                            errorMsg = $"Parentesi non corrispondenti: '{apertura.Carattere}' alla riga {apertura.Riga} e '{c}' alla riga {rigaCorrente}"
                            Return False
                        End If
                End Select
            Next
        Next

        If stackParentesi.Count > 0 Then
            Dim nonChiusa As ParentesiInfo = stackParentesi.Pop()
            errorMsg = $"Parentesi '{nonChiusa.Carattere}' aperta alla riga {nonChiusa.Riga} non è stata chiusa"
            Return False
        End If

        Return True
    End Function

    Private Function CorrispondonoParentesi(apertura As Char, chiusura As Char) As Boolean
        Return (apertura = "("c AndAlso chiusura = ")"c) OrElse
               (apertura = "["c AndAlso chiusura = "]"c) OrElse
               (apertura = "{"c AndAlso chiusura = "}"c)
    End Function

    Private Class ParentesiInfo
        Public Property Carattere As Char
        Public Property Riga As Integer
        Public Property Posizione As Integer

        Public Sub New(c As Char, r As Integer, p As Integer)
            Carattere = c
            Riga = r
            Posizione = p
        End Sub
    End Class
End Class