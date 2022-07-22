#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"
#INCLUDE "RestFul.ch"

/*/{Protheus.doc} M0601
    Fun��o respons�vel pela baixa de t�tulos recebidos da API da Precode
    @type Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    /*/
User Function M0601()
    Private lRpc	 := Type("cFilAnt") == "U"
    Private aImprime := {}
    Private aErros   := {}

	If lRpc
		RPCSetType(3)
		RpcSetEnv('01','02',,,,GetEnvServer(),{ })
        U_M0601H()
	Else 
        If cFilAnt == "01"
            Aviso("Aten��o!","Opera��o n�o permitida para a MATRIZ.",{"Ok"})
            Return
        EndIf
        U_M0601G()
    EndIf

	If lRpc
        If Len(aErros) > 0
            U_M0601L(aImprime,aErros)
        EndIf
		RpcClearEnv()
	EndIf

Return

/*/{Protheus.doc} M0601A
    Fun��o respons�vel pela consulta Rest que retorna os t�tulos dispon�veis em um ciclo espec�fico
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cData, Character, Data do ciclo
    @param cMarket, Character, Marketplace do ciclo
    @param aDetalhe, Array, T�tulos dispon�veis no ciclo
    /*/
User Function M0601A(cData,cMarket,aDetalhe,aMovimento)
    local cUrlA as char
    local cAuthA as char
    local cPathA as char
    local oRestA as object
    local cParserA as char
    local cJsonA as char
    local oJsonA as object
    local aHeaderA as array
    local nX as numeric
    local cFilFat as char
    local cCnpjCpf as char
    local cNumero as char
    local nValor as numeric
    local cParcela as char
    local nValPago as numeric
    local nValCom as numeric
    local aBaixa as array
    local aCliente as array
    local lBaixa as logical
    local lLiquida as logical
    local cPrefixo as char
    local cTipo as char
    local cMsg as char
    local cCodMkp as char
    local nY as numeric
    local cDescricao as char
    local nValRes as numeric
    local nCodMkt as numeric
    local nValIrRat as numeric
    local nTotPag as numeric
    local nPerIr as numeric
    local nValRSal as numeric
    local nValLRSa as numeric
    local nValCpc as numeric
    local aMovBan as array
    local nValMbr as numeric
    local nValMbp as numeric
    local nReserva as numeric
    local nZ as numeric

    aBaixa   := {}
    aCliente := {}
    aMovBan  := {}
    lBaixa   := .T.
    lLiquida := .T.
    cPrefixo := "1  "
    cTipo    := "NF "
    cParcela := " "
    cMsg     := ""
    nValIrRat:= 0
    nTotPag  := 0
    nPerIr   := 0
    nValRSal := 0
    nValLRSa := 0
    nValMbr  := 0
    nValMbp  := 0
    nReserva := 0
    nValCpc  := 0

    cUrlA  := "https://www.replicade.com.br"
    cPathA := "/api/v1/financeiro/titulos/" + Alltrim(cData) + "/" + Alltrim(cMarket)
    cAuthA := SuperGetMV("MV_YECOAUT",.F.,"Basic d2pxNkV0M0FDc3E2UTJ6QVI6")

    aHeaderA := {}
	Aadd(aHeaderA,'Accept: /') 
	Aadd(aHeaderA,'Content-Type: application/json; charset=ISO-8859-1')
    AADD(aHeaderA, "Authorization: " + cAuthA)

    oRestA := FWRest():new(cUrlA)
    oRestA:setPath(cPathA)
    oJsonA := JsonObject():new()

    If oRestA:Get(aHeaderA)

		If ValType(oRestA:ORESPONSEH) == "O"
            cJsonA := oRestA:GetResult()
            cJsonA := StrTran(cJsonA,"﻿","")
            cParserA  := oJsonA:FromJson(cJsonA)
		Endif

        If Empty(cParserA)
            For nX := 1 To Len(oJsonA['titulos'])
                cFilFat   := oJsonA['titulos'][nX]['filialFatura']
                cCnpjCpf  := oJsonA['titulos'][nX]['cpfCliente']
                cNumero   := StrZero(oJsonA['titulos'][nX]['notaFiscal'],9)
                nValor    := oJsonA['titulos'][nX]['valorParcela']
                nValPago  := oJsonA['titulos'][nX]['valorPago']
                If oJsonA['titulos'][nX]['valorComissao'] < 0
                    nValCom   := oJsonA['titulos'][nX]['valorComissao'] * (-1)
                Else
                    nValCom   := oJsonA['titulos'][nX]['valorComissao']
                EndIf
                cDataPgto := Strtran(oJsonA['titulos'][nX]['dataPagamento'],"-","")
                cPedPrec  := Alltrim(Str(oJsonA['titulos'][nX]['codigoPedido']))
                cCodMkp   := oJsonA['titulos'][nX]['codigoMarketplace']
                nJuros    := 0
                nValIr    := 0
                // Retorna cliente do cadastro conforme CNPJ ou CPF
                aCliente  := U_M0601C(cCnpjCpf)
                If Len(aCliente) > 0
                    // Verifica se o t�tulo est� baixado
                    lBaixa := U_M0601D(aCliente[1,1],aCliente[1,2],aCliente[1,3],cPrefixo,cNumero,cParcela,cTipo,@cMsg)
                    // Verifica se o t�tulo foi liquidado
                    lLiquida := U_M0601O(aCliente[1,1],aCliente[1,2],aCliente[1,3],cPrefixo,cNumero,cParcela,cTipo,@cMsg)
                Else 
                    MsgAlert("Cliente CPF " + cCnpjCpf + " n�o encontrado","Importa��o de ciclos")
                    Return
                EndIf
                If !Empty(cCnpjCpf)
                    nPosField := 0
                    nPosField := aScan( aBaixa, {|x| AllTrim(x[2]) == Alltrim(cCnpjCpf) .AND. AllTrim(x[3]) == Alltrim(cNumero) } )
                    If nPosField == 0
                        AADD( aBaixa, { cFilFat,cCnpjCpf,cNumero,nValor,nValPago,nValCom,cDataPgto,lBaixa,cPedPrec,lLiquida,cData,cMarket,cCodMkp,nJuros,nValIr,nValPago,aCliente[1,2],aCliente[1,3],aCliente[1,4] } )
                    Else
                        aBaixa[nPosField,4] += nValor
                        aBaixa[nPosField,5] += nValPago
                        aBaixa[nPosField,6] += nValCom
                        aBaixa[nPosField,16] += nValPago
                    EndIf
                EndIf
            Next
            For nZ := 1 to Len(aBaixa)
                nTotPag += aBaixa[nZ,16]
            Next nZ
            For nY := 1 To Len(oJsonA['resumo']['outros'])
                cDescricao:= oJsonA['resumo']['outros'][nY]['descricao']
                nValRes   := oJsonA['resumo']['outros'][nY]['valor']
                nCodMkt   := oJsonA['resumo']['outros'][nY]['codigoLancamentoMarketplace']
                If nCodMkt > 0
                    nPosField := 0
                    nPosField := aScan( aBaixa, {|x| AllTrim(x[13]) == Alltrim(Str(nCodMkt)) } )
                    If nPosField > 0
                        If aBaixa[nPosField,8] .OR. aBaixa[nPosField,10]
                            If Alltrim(cDescricao) == "Ressarcimento Promoção"
                                aBaixa[nPosField,5] += nValRes
                                aBaixa[nPosField,14] += nValRes
                            ElseIf Alltrim(cDescricao) == "Taxa Fixa" .OR. Alltrim(cDescricao) == "Comissão Ressarcimento Promoção"
                                nValRes := nValRes * (-1)
                                aBaixa[nPosField,5] -= nValRes
                                aBaixa[nPosField,6] += nValRes
                            Else 
                                If nValRes > 0
                                    nValMbr += nValRes
                                ElseIF nValRes < 0
                                    nValMbp += nValRes * (-1)
                                EndIf
                            EndIf
                        EndIf
                    Else
                        If Alltrim(cDescricao) == "Comissão de pedido cancelado"
                            nValCpc += nValRes * (-1)
                        EndIf
                    EndIf
                Else 
                    If Alltrim(cDescricao) == "Reserva de Saldo"
                        nValRSal += nValRes * (-1)
                    ElseIf Alltrim(cDescricao) == "Liberação Reserva de Saldo"
                        nValLRSa += nValRes
                    ElseIf Alltrim(cDescricao) == "Imposto de Renda"
                        nValIr += nValRes
                    Else
                        If nValRes > 0
                            nValMbr += nValRes
                        ElseIf nValRes < 0
                            nValMbp += nValRes * (-1)
                        EndIf
                    EndIf
                EndIf
            Next

            nReserva := nValRSal - nValLRSa
            
            If nValIr > 0
                AADD( aMovBan, { nValIr,"R","IR",cData,cMarket } )    // Imposto de renda
            EndIf
            
            If nValCpc > 0 
                AADD( aMovBan, { nValCpc,"P","CPC",cData,cMarket } )    // Comiss�o de pedido cancelado
            EndIf
            
            If nValMbr > 0
                AADD( aMovBan, { nValMbr,"R","LD",cData,cMarket } )     // Lan�amentos diversos a receber
            EndIf
            
            If nValMbp > 0
                AADD( aMovBan, { nValMbp,"P","LD",cData,cMarket } )     // Lan�amentos diversos a pagar
            EndIf

            If nReserva > 0
                AADD( aMovBan, { nReserva,"P","RES",cData,cMarket } )    // Reserva de saldo a pagar
            ElseIf nReserva < 0
                AADD( aMovBan, { nReserva * (-1),"R","RES",cData,cMarket } )    // Reserva de saldo a receber
            EndIf

            If Len(aBaixa) > 0
                If lRpc
                    U_M0601B(aBaixa,aMovBan)
                    If Len(aImprime) > 0 .OR. Len(aErros) > 0
                        U_M0601L(aImprime,aErros)
                    EndIf
                Else
                    aDetalhe := aClone(aBaixa)
                    aMovimento := aClone(aMovBan)
                EndIf
            EndIf
        Else
            AADD( aErros, { cData,cMarket,"Erro ao converter jSon de itens" })
        EndIf
    Else
        AADD( aErros, { cData,cMarket,oRestA:getLastError() })
    EndIf

    freeObj(oJsonA)
    freeObj(oRestA)

return

/*/{Protheus.doc} M0601B
    Fun��o respons�vel baixa e liquida��o dos t�tulos dos ciclos 
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aBaixa, Array, T�tulos que dever�o ser baixados e liquidados
    @return Logical, retorna verdadeiro caso a baixa e/ou liquida��o sejam executas corretamente.
    /*/
User Function M0601B(aBaixa,aMovBan)
    local aArea as array
    local lRet as logical
    local lBaixa as logical
    local lLiquida as logical
    local nX as numeric
    Local aCliente as array
    local cMsg as char
    local cPrefixo as char
    local cTipo as char
    local cParcela as char
    local nY as numeric
    local lMovimen as logical
    local nCount as numeric

    aArea := GetArea()
    lRet := .T.
    lBaixa := .T.
    lLiquida := .T.
    lMovimen := .T.
    cPrefixo := "1  "
    cTipo := "NF "
    cParcela := " "
    cMsg := ""
    nCount := Len(aBaixa)

    ProcRegua(nCount)

    For nX := 1 To nCount //Len(aBaixa)
            IncProc("Analisando registro " + cValToChar(nX) + " de " + cValToChar(nCount) + "...")
            aCliente := U_M0601C(aBaixa[nX,2])
            If Len(aCliente) > 0
                lBaixa := aBaixa[nX,8]
                lLiquida := aBaixa[nX,10]
                If lBaixa .AND. lLiquida
                    Begin Transaction
                        If U_M0601F(aCliente[1,1],aCliente[1,2],aCliente[1,3],cPrefixo,aBaixa[nX,3],cParcela,cTipo,aBaixa[nX,5],aBaixa[nX,6],@cMsg,aBaixa[nX,15],aBaixa[nX,9],aBaixa[nX,13])
                            lBaixa := U_M0601E(cPrefixo,aBaixa[nX,3],cParcela,cTipo,aBaixa[nX,5],aBaixa[nX,7],@cMsg,aBaixa[nX,9],aCliente[1,2],aCliente[1,3],aBaixa[nX,14],aBaixa[nX,13])
                            If lBaixa
                                AADD (aImprime, { aCliente[1,2],aCliente[1,3],cPrefixo,aBaixa[nX,3],cTipo,aBaixa[nX,5],aBaixa[nX,6],aBaixa[nX,7],"T�tulo baixado e liquidado" } )
                            Else
                                AADD( aErros, { aBaixa[nX,11],aBaixa[nX,12],cMsg } )
                            EndIf
                        Else
                            AADD( aErros, { aBaixa[nX,11],aBaixa[nX,12],cMsg } )
                        EndIf
                    End Transaction
                ElseIf lBaixa .AND. !lLiquida
                    Begin Transaction
                        If U_M0601E(cPrefixo,aBaixa[nX,3],cParcela,cTipo,aBaixa[nX,5],aBaixa[nX,7],@cMsg,aBaixa[nX,9],aCliente[1,2],aCliente[1,3],aBaixa[nX,14],aBaixa[nX,13])
                            AADD( aImprime, { aCliente[1,2],aCliente[1,3],cPrefixo,aBaixa[nX,3],cTipo,aBaixa[nX,5],aBaixa[nX,6],aBaixa[nX,7],"T�tulo Baixado" } )
                        Else
                            AADD( aErros, { aBaixa[nX,11],aBaixa[nX,12],cMsg } )
                        EndIf
                    End Transaction
                ElseIf !lBaixa .AND. lLiquida
                    Begin Transaction
                        If U_M0601F(aCliente[1,1],aCliente[1,2],aCliente[1,3],cPrefixo,aBaixa[nX,3],cParcela,cTipo,aBaixa[nX,5],aBaixa[nX,6],@cMsg,aBaixa[nX,15],aBaixa[nX,9],aBaixa[nX,13])
                            AADD( aImprime, { aCliente[1,2],aCliente[1,3],cPrefixo,aBaixa[nX,3],cTipo,aBaixa[nX,5],aBaixa[nX,6],aBaixa[nX,7],"T�tulo Liquidado" } )
                        Else
                            AADD( aErros, { aBaixa[nX,11],aBaixa[nX,12],cMsg } )
                        EndIf
                    End Transaction
                EndIf
            Else 
                AADD( aErros, { aBaixa[nX,11],aBaixa[nX,12],"Cliente n�o encontrado" } )
            EndIf
    Next nX

    If Len(aErros) == 0
        If Len(aMovBan) > 0
            For nY := 1 To Len(aMovBan)
                Begin Transaction
                    lMovimen := U_M0601Q(aMovBan[nY],@cMsg)
                    If !lMovimen
                        AADD( aErros, { aMovBan[nY,4],aMovBan[nY,5],cMsg } )
                    EndIf
                End Transaction
            Next nX
        EndIf
    EndIf

    RestArea(aArea)

Return(lRet)

/*/{Protheus.doc} M0601C
    Fun��o que retorna o cliente cadastrado conforme CNPJ/CNPJ
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cCnpjCpf, Character, CNPJ/CPF do cliente
    @return Array, retorna cadastro do cliente.
    /*/
User Function M0601C(cCnpjCpf)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local aRet   := {}

    cQuery := "SELECT A1_FILIAL, A1_COD, A1_LOJA, A1_NREDUZ "
    cQuery += "FROM " + RetSqlName("SA1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND A1_FILIAL = '" + xFilial("SA1") + "'
    cQuery += "AND A1_CGC = '" + cCnpjCpf + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty((cAlias)->A1_FILIAL)
        AADD( aRet, { (cAlias)->A1_FILIAL, (cAlias)->A1_COD , (cAlias)->A1_LOJA, (cAlias)->A1_NREDUZ } )
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(aRet)

/*/{Protheus.doc} M0601D
    Fun��o que verifica se o t�tulo j� foi baixado anteriormente.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cFilBai  , Character, filial do t�tulo verificado
    @param cCliente , Character, c�digo do cliente
    @param cLoja    , Character, c�digo da loja do cliente
    @param cPrefixo , Character, prefixo do t�tulo
    @param cNumero  , Character, n�mero do t�tulo
    @param cParcela , Character, parcela do t�tulo
    @param cTipo    , Character, tipo do t�tulo
    @param cMsg     , Character, mensagem caso encontrada baixa
    @return Logical, retorna false caso t�tulo j� tenha sido baixado anteriormente.
    /*/
User Function M0601D(cFilBai,cCliente,cLoja,cPrefixo,cNumero,cParcela,cTipo,cMsg)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local lRet   := .T.

    cQuery := "SELECT E1_BAIXA "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '" + cFilBai + "'
    cQuery += "AND E1_CLIENTE = '" + cCliente + "'
    cQuery += "AND E1_LOJA = '" + cLoja + "'
    cQuery += "AND E1_PREFIXO = '" + cPrefixo + "'
    cQuery += "AND E1_NUM = '" + cNumero + "'
    cQuery += "AND E1_PARCELA = '" + cParcela + "'
    cQuery += "AND E1_TIPO = '" + cTipo + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty(AllTrim((cAlias)->E1_BAIXA))
        lRet := .F.
        cMsg := "T�tulo baixado anteriormente"
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(lRet)

/*/{Protheus.doc} M0601E
    Fun��o que realiza o MSExecAuto da baixa do t�tulo
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cPrefixo , Character, prefixo do t�tulo
    @param cNumero  , Character, n�mero do t�tulo
    @param cParcela , Character, parcela do t�tulo
    @param cTipo    , Character, tipo do t�tulo
    @param nValPago , Numeric  , valor da baixa
    @param cDtPgto  , Character, data da baixa
    @param cMsg     , Character, mensagem caso falhe o msexecauto da baixa
    @param cPedPrec , Character, c�digo do pedido do Precode
    @param nJuros   , Numeric  , valores reembolsados pelo marketplace
    @return Logical, retorna false caso t�tulo j� tenha sido baixado anteriormente.
    /*/
User Function M0601E(cPrefixo,cNumero,cParcela,cTipo,nValPago,cDtPgto,cMsg,cPedPrec,cCliente,cLoja,nJuros,cCodMkp)
    Local aArea             := GetArea()
    Local lBaixa            := .T.
    Local aTitBaixa         := {}
    Local aBanco	        := Separa( GetNewPar("MV_YECOBCO","001/0021 /84000     "),"/")
    Local cHist             := "BAIXA AUTOMATICA E-COMMERCE"
    Local cMotBai           := "NOR"
    Local nI
    Local aErro             := {}
    Private lMsErroAuto	    := .F.
    Private lAutoErrNoFile  := .T.

    dbSelectArea("SA6")
    SA6->(dbGotop())
    SA6->(dbSetOrder(1))
    SA6->(dbSeek(xFilial("SA6")+aBanco[1]+aBanco[2]+aBanco[3]))

    Aadd(aTitBaixa, {"E1_PREFIXO"  , cPrefixo       , nil})
    Aadd(aTitBaixa, {"E1_NUM"      , cNumero        , nil})
    Aadd(aTitBaixa, {"E1_PARCELA"  , cParcela       , nil})
    Aadd(aTitBaixa, {"E1_TIPO"     , cTipo          , nil})
    Aadd(aTitBaixa, {"E1_CLIENTE"  , cCliente       , nil})
    Aadd(aTitBaixa, {"E1_LOJA"     , cLoja          , nil})
    Aadd(aTitBaixa, {"AUTBANCO"    , SA6->A6_COD    , nil})
    Aadd(aTitBaixa, {"AUTAGENCIA"  , SA6->A6_AGENCIA, nil})
    Aadd(aTitBaixa, {"AUTCONTA"    , SA6->A6_NUMCON , nil})
    Aadd(aTitBaixa, {"AUTJUROS"    , 0              , nil,.T.})
    Aadd(aTitBaixa, {"AUTMULTA"    , ABS(nJuros)    , nil})
    Aadd(aTitBaixa, {"AUTVALREC"   , ABS(nValPago)  , nil})
    Aadd(aTitBaixa, {"AUTMOTBX"    , cMotBai        , nil})
    Aadd(aTitBaixa, {"AUTDTBAIXA"  , STOD(cDtPgto)  , nil})
    Aadd(aTitBaixa, {"AUTDTCREDITO", STOD(cDtPgto)  , nil})
    Aadd(aTitBaixa, {"AUTHIST"     , cHist          , nil})

    MSExecAuto({|x,y| Fina070(x,y)},aTitBaixa,3)
    If lMsErroAuto
        cMsg := "T�tulo : " + cNumero + CRLF
        cMsg += "ID Pedido : " + cPedPrec + CRLF
        cMsg += "Cod. Marketplace : " + cCodMkp + CRLF
        lBaixa := .F.
        aErro := GetAutoGRLog()
        For nI := 1 To Len(aErro)
            cMsg += aErro[nI] + CRLF
        Next nI
    Else
        dbSelectArea("SE1")
        SE1->(dbSetOrder(1))
        If (SE1->(dbSeek("02"+cPrefixo+cNumero+cParcela+cTipo)))
            RecLock("SE1",.F.)
            SE1->E1_YPEDPRE := cPedPrec
            SE1->E1_IDWARE  := cCodMkp
            SE1->(MsUnLock())
        EndIf
        SE1->(dbCloseArea())
    EndIf

    SA6->(dbCloseArea())

    RestArea(aArea)

Return(lBaixa)

/*/{Protheus.doc} M0601F
    Fun��o que realiza o MSExecAuto da liquida��o do t�tulo
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cFilLiq  , Character, filial do t�tulo
    @param cCliLiq  , Character, c�digo do cliente
    @param cLojLiq  , Character, loja do cliente
    @param cPreLiq  , Character, prefixo do t�tulo
    @param cNumLiq  , Character, n�mero do t�tulo
    @param cParLiq  , Character, parcela do t�tulo
    @param cTipLiq  , Character, tipo do t�tulo
    @param nValPago , Character, valor da baixa
    @param nValLiq  , Character, valor da comiss�o (valor do t�tulo de liquida��o)
    @param cMsg     , Character, mensagem de resultado do processamento do MSExecAuto
    @return Logical, retorna resultado do MSExcAuto de liquida��o.
    /*/
User Function M0601F(cFilLiq,cCliLiq,cLojLiq,cPreLiq,cNumLiq,cParLiq,cTipLiq,nValPago,nValLiq,cMsg,nValIr,cPedPrec,cCodMkp)
    Local aArea             := GetArea()
    Local aCab              := {}
    Local aTit              := {}
    Local aItens            := {}
    Local cFiltro           := ""
    Local lRet              := .T.
    Local cCond             := "300"
    Local aBanco	        := Separa( GetNewPar("MV_YECOBCO","001/0021 /84000     "),"/")
    Local cNatureza         := '020100    '
    Local nMoeda            := 1
    Local nI
    Private lMsErroAuto     := .F.
    Private lAutoErrNoFile  := .T. 

    dbSelectArea("SA6")
    SA6->(dbGotop())
    SA6->(dbSetOrder(1))
    SA6->(dbSeek(xFilial("SA6")+aBanco[1]+aBanco[2]+aBanco[3]))

    cFiltro := "E1_FILIAL == " + valtosql(xFilial("SE1")) + " .AND. "
    cFiltro += "E1_PREFIXO == " + valtosql(cPreLiq) + " .AND. "
    cFiltro += "E1_NUM == " + valtosql(cNumLiq) + " .AND. "
    cFiltro += "E1_TIPO == " + valtosql(cTipLiq) + " .AND. "
    cFiltro += "E1_CLIENTE == " + valtosql(cCliLiq) + " .AND. "
    cFiltro += "E1_LOJA == " + valtosql(cLojLiq) + " .AND. "
    cFiltro += "E1_NUMLIQ == " 	+ ValToSql( CriaVar("E1_NUMLIQ",.F.) ) + " .AND. "
    cFiltro += "E1_SITUACA $ '0FG' .And. E1_SALDO > 0  "

    aAdd( aCab, {"cCondicao" , cCond     })
    aAdd( aCab, {"cNatureza" , cNatureza })
    aAdd( aCab, {"E1_TIPO"   , cTipLiq   })
    aAdd( aCab, {"cCLIENTE"  , cCliLiq   })
    aAdd( aCab, {"nMoeda"    , nMoeda    })
    aAdd( aCab, {"cLOJA"     , cLojLiq   })

    Aadd(aTit, {"E1_PREFIXO"	, cPreLiq			})
    Aadd(aTit, {"E1_NUM" 		, cNumLiq   		})
    Aadd(aTit, {"E1_PARCELA"    , "L"               })
    Aadd(aTit, {"E1_TIPO"		, cTipLiq			})
    Aadd(aTit, {"E1_PORTADO" 	, SA6->A6_COD    	})
    Aadd(aTit, {"E1_AGEDEP" 	, SA6->A6_AGENCIA	})
    Aadd(aTit, {"E1_NUMBCO" 	, SA6->A6_NUMCON   	})
    Aadd(aTit, {"E1_VENCTO" 	, ddatabase 		})
    Aadd(aTit, {"E1_EMISSAO" 	, ddatabase 		})
    Aadd(aTit, {"E1_VLCRUZ" 	, nValLiq   		})
    Aadd(aTit, {"E1_HIST" 		, "E-COMMERCE" 		})
    Aadd(aTit, {"E1_ORIGEM"  	, "FINA460" 		})
    Aadd(aTit, {"E1_ACRESC" 	, 0 				})
    Aadd(aTit, {"E1_DECRESC" 	, 0     			})

    aAdd(aItens, aTit )

    msExecAuto( { |a,b,c,d,e| FINA460(a,b,c,d,e) }, Nil, aCab, aItens, 3, cFiltro )
	If lMsErroAuto
        cMsg := "T�tulo : " + cNumLiq + CRLF
        cMsg += "ID Pedido : " + cPedPrec + CRLF
        cMsg += "Cod. Marketplace : " + cCodMkp + CRLF
        lRet := .F.
        aErro := GetAutoGRLog()
        For nI := 1 To Len(aErro)
            cMsg += aErro[nI] + CRLF
        Next nI
    EndIf

    If lRet
        cMsg := "T�tulo " + cNumLiq + " baixado e liquidado com sucesso !!!"
    EndIf

    SA6->(dbCloseArea())

    RestArea(aArea)

Return(lRet)

/*/{Protheus.doc} M0601G
    Fun��o que monta tela com os ciclos dispon�veis na API
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    /*/
User Function M0601G()
    Local aFields   := {" ", "Data do Ciclo", "Marktplace", "Data da Baixa" }
    Local aButtons  := {}
    Local aItens    := {}
    Local oOk       := LoadBitMap(GetResources(), "LBOK")
    Local oNo       := LoadBitMap(GetResources(), "LBNO")

    U_M0601H(@aItens)

    If Len(aItens) > 0
        aSort( aItens,,, { |x,y| x[2]+x[3] > y[2]+y[3] } )
        
        oDlg := FWDialogModal():New()

        oDlg:SetEscClose(.T.)
        oDlg:SetTitle("Importa��o de Ciclos")
        
        //Seta a largura e altura da janela em pixel
        oDlg:setSize(250, 410)

        oDlg:CreateDialog()
        oDlg:addCloseButton(nil, "Fechar")
        oContainer := TPanel():New( ,,, oDlg:getPanelMain() )
        oContainer:Align := CONTROL_ALIGN_ALLCLIENT

        cLine := "{If(aItens[oListBox:nAt,1],oOk,oNo),aItens[oListBox:nAT][2],aItens[oListBox:nAT][3],aItens[oListBox:nAT][4]}"
        bLine := &( "{ || " + cLine + " }" )

        oListBox:=TWBrowse():New( 001,001,400,200,,aFields,,oContainer,,,,,,,,,,,,.F.,,.T.,,.F.,,,)
        oListBox:SetArray(aItens)
        oListBox:bLDblClick := { || aItens[oListBox:nAt,1] := !aItens[oListBox:nAt,1] }
        oListBox:bRClicked := { || U_M0601R(aItens,oListBox,oListBox:nAT) }
        oListBox:lHScroll = .F.
        oListBox:lVScroll = .T.
        oListBox:bLine := bLine

        oDlg:AddButton( 'Confirmar'	,{|| Processa( { || U_M0601J(aItens) } ) ,oDlg:DeActivate() }, 'Confirmar' , , .T., .F., .T., )
        oDlg:AddButton( 'Visualizar',{|| U_M0601I(aItens[oListBox:nAT][2],aItens[oListBox:nAT][3])}, 'Visualizar' , , .T., .F., .T., )

        oDlg:addButtons(aButtons)

        oDlg:Activate()

    EndIf

Return

/*/{Protheus.doc} M0601H
    Fun��o que retorna t�tulos dispon�veis no ciclo.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aItens, Array, array com os t�tulos dispon�veis no ciclo.
    /*/
User Function M0601H(aItens)
    local cUrl as char
    local cAuth as char
    local cPath as char
    local oRest as object
    local cParser as char
    local cJson as char
    local oJson as object
    local cData as char
    local cMarket as char
    local aHeader as array
    local nX as numeric
    local dData as date
    
    cUrl  := "https://www.replicade.com.br"
    cPath := "/api/v1/financeiro/ciclos"
    
    cAuth := SuperGetMV("MV_YECOAUT",.F.,"Basic d2pxNkV0M0FDc3E2UTJ6QVI6")

    aHeader := {}
	Aadd(aHeader,'Accept: /') 
	Aadd(aHeader,'Content-Type: application/json; charset=ISO-8859-1')
    AADD(aHeader, "Authorization: " + cAuth)

    oRest := FWRest():new(cUrl)
    oRest:setPath(cPath)
    oJson := JsonObject():new()

    If oRest:Get(aHeader)

		If ValType(oRest:ORESPONSEH) == "O"
            cJson := oRest:GetResult()
            cJson := StrTran(cJson,"﻿","")
            cParser  := oJson:FromJson(cJson)
		Endif

        If Empty(cParser)
            For nX := 1 To Len(oJson['ciclos'])
                cData := oJson['ciclos'][nX]['data']
                cMarket := oJson['ciclos'][nX]['marketplace']
                If !Empty(cData) .AND. !Empty(cMarket)
                    If lRpc
                        dData := STOD(SubStr(cData,1,4) + SubStr(cData,6,2) + SubStr(cData,9,2))
                        If dData == ddatabase
                            U_M0601A(cData,cMarket)
                        Else
                            AADD (aErros, { cData,cMarket,"A data base do sistema � diferente da data do ciclo." })
                        EndIf
                    Else 
                        AADD( aItens, {.F., DTOC(STOD(StrTran(cData,"-",""))), cMarket, DTOC(STOD(StrTran(cData,"-",""))) })
                    EndIf
                EndIf
            Next nX
        Else
            AADD( aErros, { cData,cMarket,"Erro ao converter jSon dos ciclos" })
        EndIf
    Else
        AADD( aErros, { cData,cMarket,oRest:getLastError() })
    EndIf

    freeObj(oJson)
    freeObj(oRest)

Return

/*/{Protheus.doc} M0601I
    Fun��o que retorna t�tulos dispon�veis no ciclo.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aItens, Array, array com os t�tulos dispon�veis no ciclo.
    /*/
User Function M0601I(cData,cMarket)
    Local aFields   := {" ","Cliente","Loja","Nome", "Nota Fiscal", "Valor", "Valor Pago", "Valor Comiss�o", "Data Pagamento", "C�digo Marketplace" }
    Local aDetalhe  := {}
    Local aMovimento:= {}
    Local oOK       := LoadBitmap(GetResources(),'BR_VERDE')
    Local oNO       := LoadBitmap(GetResources(),'BR_VERMELHO')
    Local oSL       := LoadBitMap(GetResources(), "BR_AMARELO")
    Local oSB       := LoadBitMap(GetResources(), "BR_AZUL")
    Local lHasButton:= .T.
    Local nValTot   := 0
    Local nPagTot   := 0
    Local nComTot   := 0
    Local nX
    Local oSay1, oSay2, oSay3, oSay4, oSay5, oSay6
    Local oFont     := TFont():New('Courier new',,-14,.T.)
    Local oFont1    := TFont():New('Courier new',,-14,.T.)


    aImprime := {}
    aErros   := {}
    oFont1:Bold := .T.
    
    cData := SubStr(cData,7,4) + "-" + SubStr(cData,4,2) + "-" + SubStr(cData,1,2)
    
    U_M0601A(cData,cMarket,@aDetalhe,@aMovimento)

    If Len(aDetalhe) > 0
        For nX := 1 to Len(aDetalhe)
            nValTot += aDetalhe[nX,4]
            nPagTot += aDetalhe[nX,5]
            nComTot += aDetalhe[nX,6]
        Next nX
        
        oDlgDet := FWDialogModal():New()

        oDlgDet:SetEscClose(.T.)
        oDlgDet:SetTitle("Detalhes do Ciclo")
        
        //Seta a largura e altura da janela em pixel
        oDlgDet:setSize(300, 660)

        oDlgDet:CreateDialog()
        oDlgDet:addCloseButton(nil, "Fechar")
        oContDet := TPanel():New( ,,, oDlgDet:getPanelMain() )
        oContDet:Align := CONTROL_ALIGN_ALLCLIENT

        cLineDet := "{U_M0601P(aDetalhe[oLBDet:nAT][8],aDetalhe[oLBDet:nAT][10]),aDetalhe[oLBDet:nAT][17],aDetalhe[oLBDet:nAT][18],aDetalhe[oLBDet:nAT][19],aDetalhe[oLBDet:nAT][3],Transform(aDetalhe[oLBDet:nAT][4],PesqPict('SE1','E1_VALOR')),Transform(aDetalhe[oLBDet:nAT][5],PesqPict('SE1','E1_VALOR')),Transform(aDetalhe[oLBDet:nAT][6],PesqPict('SE1','E1_VALOR')),DTOC(STOD(aDetalhe[oLBDet:nAT][7])),aDetalhe[oLBDet:nAT][13] }"
        bLineDet := &( "{ || " + cLineDet + " }" )

        oLBDet:=TWBrowse():New( 020,001,650,240,,aFields,,oContDet,,,,,,,,,,,,.F.,,.T.,,.F.,,,)
        oLBDet:SetArray(aDetalhe)
        oLBDet:bLDblClick := { || U_M0601M() }
       	oSay1 := TSay():New(001,001,{||"Total :"},oContDet,,oFont,,,,.T.,,,,,,,,,,lHasButton)
    	oSay2 := TSay():New(001,030,{||Transform(nValTot,"@E 999,999,999.99")},oContDet,,oFont1,,,,.T.,,,,,,,,,,lHasButton)
       	oSay3 := TSay():New(001,110,{||"Total Pago :"},oContDet,,oFont,,,,.T.,,,,,,,,,,lHasButton)
    	oSay4 := TSay():New(001,150,{||Transform(nPagTot,"@E 999,999,999.99")},oContDet,,oFont1,,,,.T.,,,,,,,,,,lHasButton)
       	oSay5 := TSay():New(001,230,{||"Total Comiss�o:"},oContDet,,oFont,,,,.T.,,,,,,,,,,lHasButton)
    	oSay6 := TSay():New(001,290,{||Transform(nComTot,"@E 999,999,999.99")},oContDet,,oFont1,,,,.T.,,,,,,,,,,lHasButton)

        oLBDet:bLine := bLineDet

        oDlgDet:Activate()

    EndIf

Return

/*/{Protheus.doc} M0601J
    Fun��o que executa a fun��o do bot�o confirma da tela de ciclos.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aItens, Array, array com os t�tulos dispon�veis no ciclo.
    /*/
User Function M0601J(aItens)
    Local nX
    Local aDetalhe := {}
    Local aMovimento := {}
    Local nOpc     := 0
    Local cAvisoTit := "T�tulos baixados"
    Local cAvisoMsg := "Deseja imprimir o relat�rio com as informa��es dos t�tulos baixados pela rotina ?"
    Local cErroMsg  := "Deseja mostrar log de erros ?"

    For nX := 1 To Len(aItens)
        If aItens[nX,1]
            cData := SubStr(aItens[nX,4],7,4) + "-" + SubStr(aItens[nX,4],4,2) + "-" + SubStr(aItens[nX,4],1,2)
            if CTOD(aItens[nX,2]) == ddatabase
                U_M0601A(cData,aItens[nX,3],@aDetalhe,@aMovimento)
            Else
                AADD(aErros, { cData, aItens[nX,3] , "A data base do sistema � diferente da data do ciclo."} )
            EndIf
            If Len(aDetalhe) > 0
                U_M0601B(aDetalhe,aMovimento)
            EndIf
        endIf
    Next nX

    If Len(aImprime) > 0
        nOpc := Aviso( cAvisoTit, cAvisoMsg, { "Sim", "N�o"} , 2)
        If nOpc == 1
            U_M0601K(aImprime)
        Else
            Return
        EndIf
    EndIf

    If Len(aErros) > 0
        nOpc := Aviso( cAvisoTit, cErroMsg, { "Sim", "N�o"} , 2)
        If nOpc == 1
            U_M0601N(aErros)
        Else
            Return
        EndIf
    EndIf

Return

/*/{Protheus.doc} M0601K
    Fun��o para execu��o do relat�rios do processamento da baixa dos t�tulos
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aImprime, Array, array com os t�tulos processados (baixados ou liquidados) do ciclo.
    /*/
User Function M0601K(aImprime)
	Local oReport

    oReport := M0601KA(aImprime)
    oReport:PrintDialog()	

Return

/*/{Protheus.doc} M0601KA
    Fun��o para execu��o do relat�rios do processamento da baixa dos t�tulos
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aImprime, Array, array com os t�tulos processados (baixados ou liquidados) do ciclo.
    /*/
Static Function M0601KA(aImprime)
	Local oReport
	Local oSection1

	oReport := TReport():New("BAIXAS","Relat�rio de t�tulos baixados","BAIXAS",{|oReport| M0601KB(oReport,aImprime)},"Relat�rio de t�tulos baixados")

	oReport:nFontBody := 9
	oReport:SetPortrait()

	oSection1 := TRSection():New(oReport,"Baixas",)

	TRCell():New(oSection1,"CODCLI"	    ," ","C�d.Cliente",,15)
	TRCell():New(oSection1,"LOJCLI"  	," ","Loja Cliente",,15)
	TRCell():New(oSection1,"PREFIXO"  	," ","Prefixo",,10)
	TRCell():New(oSection1,"NUMERO"  	," ","N� T�tulo",,20)
	TRCell():New(oSection1,"TIPO"     	," ","Tipo",,10)
    TRCell():New(oSection1,"VALPAG"	    ," ","Vlr Pago","@E 999,999,999.99",15)
    TRCell():New(oSection1,"VALCOM"	    ," ","Vlr Comiss�o","@E 999,999,999.99",15)
	TRCell():New(oSection1,"DATA"     	," ","Data Pagamento",,20)
    TRCell():New(oSection1,"TPBAIXA"   	," ","Tipo da Baixa",,35)

Return oReport

/*/{Protheus.doc} M0601KB
    Fun��o para execu��o do relat�rios do processamento da baixa dos t�tulos
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aImprime, Array, array com os t�tulos processados (baixados ou liquidados) do ciclo.
    /*/
Static Function M0601KB(oReport,aImprime)
	Local oSection1 := oReport:Section(1)
	Local nX

	oSection1:Init()

	For nX := 1 To Len(aImprime)
		oSection1:Cell("CODCLI"):SetValue(aImprime[nX,1])
		oSection1:Cell("LOJCLI"):SetValue(aImprime[nX,2])
		oSection1:Cell("PREFIXO"):SetValue(aImprime[nX,3])
		oSection1:Cell("NUMERO"):SetValue(aImprime[nX,4])
		oSection1:Cell("TIPO"):SetValue(aImprime[nX,5])
		oSection1:Cell("VALPAG"):SetValue(aImprime[nX,6])
		oSection1:Cell("VALCOM"):SetValue(aImprime[nX,7])
		oSection1:Cell("DATA"):SetValue(DTOC(STOD(aImprime[nX,8])))
        oSection1:Cell("TPBAIXA"):SetValue(aImprime[nX,9])
		oSection1:PrintLine()
	Next nX

	oReport:SkipLine()
	oReport:ThinLine()

	oSection1:Finish()

Return

/*/{Protheus.doc} M0601L
    Fun��o para monta e=mail para envio ap�s processamento dos t�tulos via schedule
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aImprime, Array, array com os t�tulos processados (baixados ou liquidados) do ciclo.
    @param aErros  , Array, array com as descri��es dos erros de processamento do ciclo.
    /*/
User Function M0601L(aImprime,aErros)
    Local aEmail    := STRTOKARR(GetMv("MV_YEMPREC", .F., "000000" ),';')
    Local cEmail	:= ""
    Local cSubject	:= "PRECODE - T�tulos baixados automaticamente"
    Local cMsg		:= ""
    Local lImprime  := .F.
    Local nX, nY

    For nY := 1 TO LEN(aEmail)
        cEmail += UsrRetMail(aEmail[nY]) + IIF(nY < Len(aEmail),";","")
    Next

	If Len(aImprime) > 0
        lImprime := .T.
        cMsg := '<p>Baixa autom�tica de t�tulos a receber - PRECODE</p>'
        cMsg += '<html>'
        cMsg += '<body>'
        cMsg += '<form>'
        cMsg += '<table border="1" align="left" cellpadding="5" cellspacing="0">'
        cMsg += '<tr>'
        cMsg += '<td align="center"<b>Cliente</b></td>'
        cMsg += '<td align="center"<b>Loja</b></td>'
        cMsg += '<td align="center"<b>Prefixo</b></td>'
        cMsg += '<td align="center"<b>N�mero</b></td>'
        cMsg += '<td align="center"<b>Tipo</b></td>'
        cMsg += '<td align="center"<b>Valor Pago</b></td>'
        cMsg += '<td align="center"<b>Valor Comiss�o</b></td>'
        cMsg += '<td align="center"<b>Data Pagamento</b></td>'
        cMsg += '<td align="center"<b>Tipo</b></td>'
        cMsg += '</tr>'
        For nX := 1 To Len(aImprime)
            cMsg += '<tr>'
            cMsg += '<td align="center">' + aImprime[nX,1] + '</td>'
            cMsg += '<td align="center">' + aImprime[nX,2] + '</td>'
            cMsg += '<td align="center">' + aImprime[nX,3] + '</td>'
            cMsg += '<td align="center">' + aImprime[nX,4] + '</td>'
            cMsg += '<td align="center">' + aImprime[nX,5] + '</td>'
            cMsg += '<td align="center">' + Transform(aImprime[nX,6],PesqPict("SE1","E1_VALOR")) + '</td>'
            cMsg += '<td align="center">' + Transform(aImprime[nX,7],PesqPict("SE1","E1_VALOR")) + '</td>'
            cMsg += '<td align="center">' + DTOC(STOD(aImprime[nX,8])) + '</td>'
            cMsg += '<td align="center">' + aImprime[nX,9] + '</td>'
            cMsg += '</tr>'
        Next nX
        cMsg += '</table>'
        cMsg += '</form>'
        cMsg += '</body>'
        cMsg += '</html>'
    EndIf

	If Len(aErros) > 0
        If lImprime
            cMsg += '<br>'
            cMsg += '<br>'
            cMsg += '<br>'
            cMsg += '<p>Problemas na Baixa autom�tica de t�tulos a receber - PRECODE</p>'
        Else
            cMsg := '<p>Problemas na Baixa autom�tica de t�tulos a receber - PRECODE</p>'
        EndIf
        cMsg += '<html>'
        cMsg += '<body>'
        cMsg += '<form>'
        cMsg += '<table border="1" align="left" cellpadding="5" cellspacing="0">'
        cMsg += '<tr>'
        cMsg += '<td align="center"<b>Ciclo</b></td>'
        cMsg += '<td align="center"<b>Data</b></td>'
        cMsg += '<td align="center"<b>Erro</b></td>'
        cMsg += '</tr>'
        For nX := 1 To Len(aErros)
            cMsg += '<tr>'
            cMsg += '<td align="center">' + SubStr(aErros[nX,1],9,2) + "/" + SubStr(aErros[nX,1],6,2) + "/" + SubStr(aErros[nX,1],1,4) + '</td>'
            cMsg += '<td align="center">' + aErros[nX,2] + '</td>'
            cMsg += '<td align="center">' + aErros[nX,3] + '</td>'
            cMsg += '</tr>'
        Next nX
        cMsg += '</table>'
        cMsg += '</form>'
        cMsg += '</body>'
        cMsg += '</html>'
    EndIf

	U_M0601LA(cEmail,cSubject,cMsg)

Return

/*/{Protheus.doc} M0601LA
    Fun��o que envia e=mail ap�s processamento dos t�tulos via schedule
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cEmail   , Character, contas que receber�o o e-mail de processamento da baixa de t�tulos Precode
    @param cSubject , Character, assunto
    @param cMsg     , Character, html contendo as informa��es dos t�tulos que foram processados ou erros que ocorreram durante o processamento da baixa de t�tulos Precode
    /*/
User Function M0601LA(cEmail,cSubject,cMsg)
    Local oServer  	:= TMailManager():New()
    Local oMessage 	:= TMailMessage():New()
    Local cServer  	:= ALLTRIM(GetMv("MV_RELSERV"))
    Local cAccount 	:= ALLTRIM(GetMv("MV_RELACNT"))
    Local cPass		:= ALLTRIM(GetMv("MV_RELPSW"))
    Local nErro
    Local nPosPort  := 0

    nPosPort := AT(":", cServer)

    cServer := SubStr(cServer,1,nPosPort-1)

    oServer:SetUseTLS(.T.) // Ativa a utiliza��o da criptografia TLS
    oServer:Init( "", cServer, cAccount, cPass, 0, 587 ) 
	
	If( (nErro := oServer:SmtpConnect()) != 0 )
      conout( "N�o conectou.", oServer:GetErrorString( nErro ) )
      Return
   EndIf
   
   If( (nErro := oServer:smtpAuth(cAccount, cPass)) != 0)
	   conout("Falha ao autenticar: " + oServer:getErrorString(nErro))
	   oServer:smtpDisconnect()
	   Return .F.
   EndIf

	oMessage:Clear()
	oMessage:cFrom 		:= cAccount
	oMessage:cTo 	  	:= ""
	oMessage:cCc 		:= ""
	oMessage:cBcc 		:= cEmail
	oMessage:cSubject	:= cSubject
	oMessage:cBody 		:= cMsg
		
   If( (nErro := oMessage:Send( oServer )) != 0 )
      conout( "N�o enviou o e-mail.", oServer:GetErrorString( nErro ) )
	   Return
   EndIf
      
   If( (nErro := oServer:SmtpDisconnect()) != 0 )
      conout( "N�o desconectou.", oServer:GetErrorString( nErro ) )
      Return
   EndIf
      
Return

/*/{Protheus.doc} M0601M
    Fun��o que exibe a leganda dos detalhes dos ciclos.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    /*/
User Function M0601M()
    local oLegend as object
    
    oLegend := FWLegend():New()
    
    oLegend:Add("", "BR_VERDE"      , "T�tulo em aberto")
    oLegend:Add("", "BR_AZUL"       , "T�tulo baixado sem Liquida��o")
    oLegend:Add("", "BR_AMARELO"    , "T�tulo liquidado sem baixa")
    oLegend:Add("", "BR_VERMELHO"   , "T�tulo baixado")
    
    oLegend:Activate()
    oLegend:View()
    
    oLegend:Deactivate()
    
    FreeObj(oLegend)
 
Return

/*/{Protheus.doc} M0601N
    Fun��o que exibe log de erros quando rotina de baixa de t�tulos Precode � executada a partir do menu
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aErros, array, array contendo as mensagens dos erros encontrados durante o processamento
    /*/
User Function M0601N(aErros)
    Local oFntTxt := TFont():New("Calibri",,-14,,.F.,,,,,.F.,.F.)
    Local cTitulo := "Log de erro"
    Local cMsg    := ""
    Local nX

    For nX := 1 To Len(aErros)
        cData := SubStr(aErros[nX,1],9,2) + "/" + SubStr(aErros[nX,1],6,2) + "/" + SubStr(aErros[nX,1],1,4)
        cMsg += "Ciclo : " + cData + " - " + aErros[nX,2] + CRLF
        cMsg += "Problema : " + aErros[nX,3] + CRLF
        cMsg += + CRLF
    Next nX

    DEFINE MSDIALOG oDlgMens TITLE cTitulo FROM 000, 000  TO 300, 600 COLORS 0, 16777215 PIXEL
        @ 002, 004 GET oMsg VAR cMsg OF oDlgMens MULTILINE SIZE 391, 121 FONT oFntTxt COLORS 0, 16777215 HSCROLL PIXEL
        @ 127, 134 BUTTON oBtnOk  PROMPT "&Ok" SIZE 051, 019 ACTION oDlgMens:End() OF oDlgMens PIXEL
    ACTIVATE MSDIALOG oDlgMens CENTERED

Return

/*/{Protheus.doc} M0601O
    Fun��o que verifica se o t�tulo j� foi liquidado anteriormente.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param cFilBai  , Character, filial do t�tulo verificado
    @param cCliente , Character, c�digo do cliente
    @param cLoja    , Character, c�digo da loja do cliente
    @param cPrefixo , Character, prefixo do t�tulo
    @param cNumero  , Character, n�mero do t�tulo
    @param cParcela , Character, parcela do t�tulo
    @param cTipo    , Character, tipo do t�tulo
    @param cMsg     , Character, mensagem caso encontrada baixa
    @return Logical, retorna false caso t�tulo j� tenha sido liquidado anteriormente.
    /*/
User Function M0601O(cFilBai,cCliente,cLoja,cPrefixo,cNumero,cParcela,cTipo,cMsg)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local lRet   := .T.
    
    cParcela := "L"

    cQuery := "SELECT COUNT(*) AS LIQUIDA "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '" + cFilBai + "'
    cQuery += "AND E1_CLIENTE = '" + cCliente + "'
    cQuery += "AND E1_LOJA = '" + cLoja + "'
    cQuery += "AND E1_PREFIXO = '" + cPrefixo + "'
    cQuery += "AND E1_NUM = '" + cNumero + "'
    cQuery += "AND E1_PARCELA = '" + cParcela + "'
    cQuery += "AND E1_TIPO = '" + cTipo + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If (cAlias)->LIQUIDA > 0
        lRet := .F.
        cMsg := "T�tulo liquidado anteriormente"
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(lRet)

/*/{Protheus.doc} M0601P
    Fun��o que define status de cada item do detalhe de um ciclo.
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param lBaixa, Logical, indica se o t�tulo pode ser baixado.
    @param lBaixa, Logical, indica se o t�tulo pode ser liquidado.
    @return Object, retorna o objeto bitmap com a cor correspondente conforme as regras de status definidas.
    /*/
User Function M0601P(lBaixa,lLiquida)
    Local oOK       := LoadBitmap(GetResources(),'BR_VERDE')
    Local oNO       := LoadBitmap(GetResources(),'BR_VERMELHO')
    Local oSL       := LoadBitMap(GetResources(), "BR_AMARELO")
    Local oSB       := LoadBitMap(GetResources(), "BR_AZUL")
    Local oCor

    If lBaixa .AND.lLiquida
        oCor := oOK
    ElseIf !lBaixa .AND. !lLiquida
        oCor := oNO
    ElseIf !lBaixa .AND. lLiquida
        oCor := oSB
    ElseIf lBaixa .AND. !lLiquida
        oCor := oSL
    EndIf

Return(oCor)

/*/{Protheus.doc} M0601P
    Fun��o respons�vel pela inclus�o de movimento banc�rio via MsExecAuto
    @type  Function
    @author Pono
    @since 06/12/2021
    @version 12.1.23+
    @param aMovBan , Array    , array com as informa��es do movimentos banc�rios.
    @param cMsg    , Character, vari�vel que recebe a mensagem de retorno do processamento do MsExecAuto.
    @return Logical, retorna um valor l�gico indicando se o processamento ocorreu sem erros ou n�o.
    /*/
User Function M0601Q(aMovBan,cMsg)
    Local aArea             := GetArea()
    Local aFina100          := {}
    Local lMovime           := .T.
    Local aBanco	        := Separa( GetNewPar("MV_YECOBCO","001/0021 /84000     "),"/")
    Local cHist             := ""
    Local cNatureza         := ""
    Local cMoeda            := "M1"
    Local nOpcao            := 3
    Local nI
    Private lMsErroAuto	    := .F.
    Private lAutoErrNoFile  := .T. 

    If aMovBan[2] == "R"
        nOpcao := 4
    EndIf

    If aMovBan[3] == "CPC"
        cNatureza := SuperGetMV("MV_YNMVCPC",.F.,"020100    ")
        cHist := "COMISSAO PEDIDO CANCELADO"
        cTitulo := "CPC" + StrTran(aMovBan[4],"-","") + aMovBan[5]
    ElseIf aMovBan[3] == "LD"
        cNatureza := SuperGetMV("MV_YNMVLG",.F.,"020100    ")
        cHist := "LANCAMENTOS DIVERSOS"
        cTitulo := "LD" + StrTran(aMovBan[4],"-","") + aMovBan[5]
    ElseIf aMovBan[3] == "RES"
        cNatureza := SuperGetMV("MV_YNMVRES",.F.,"020100    ")
        cHist := "RESERVA DE SALDO"
        cTitulo := "RES" + StrTran(aMovBan[4],"-","") + aMovBan[5]
    ElseIf aMovBan[3] == "IR"
        cNatureza := SuperGetMV("MV_YNMVIR",.F.,"020100    ")
        cHist := "IMPOSTO DE RENDA"
        cTitulo := "IR" + StrTran(aMovBan[4],"-","") + aMovBan[5]
    EndIf
    
    dbSelectArea("SE5")
    SE5->(dbSetOrder(23))
    If !(SE5->(dbSeek(xFilial("SE5")+cTitulo)))

        dbSelectArea("SA6")
        SA6->(dbGotop())
        SA6->(dbSetOrder(1))
        SA6->(dbSeek(xFilial("SA6")+aBanco[1]+aBanco[2]+aBanco[3]))

        aFina100 := {{"E5_DATA"       ,dDataBase                ,Nil},;
                    {"E5_MOEDA"       ,cMoeda                   ,Nil},;
                    {"E5_VALOR"       ,aMovBan[1]               ,Nil},;
                    {"E5_NATUREZ"     ,cNatureza                ,Nil},;
                    {"E5_BANCO"       ,SA6->A6_COD              ,Nil},;
                    {"E5_AGENCIA"     ,SA6->A6_AGENCIA          ,Nil},;
                    {"E5_CONTA"       ,SA6->A6_NUMCON           ,Nil},;
                    {"E5_VENCTO"      ,dDataBase                ,Nil},;
                    {"E5_BENEF"       ,"E-COMMERCE"             ,Nil},;
                    {"E5_HISTOR"      ,cHist                    ,Nil},;
                    {"E5_YIDPREC"     ,cTitulo                  ,Nil},;
                    {"NCTBONLINE"     ,2                        ,Nil}}     //1=Sim;2=N�o
        
        msExecAuto({|x,y,z| Fina100(x,y,z)},0,aFina100,nOpcao)

        If lMsErroAuto
            cMsg := "T�tulo : " + cTitulo + CRLF
            lMovime := .F.
            aErro := GetAutoGRLog()
            For nI := 1 To Len(aErro)
                cMsg += aErro[nI] + CRLF
            Next nI
        EndIf

        SA6->(dbCloseArea())

    EndIf

    SE5->(dbCloseArea())

    RestArea(aArea)

Return(lMovime)

User Function M0601R(aItens,oListBox,nLin)
    Local lRet := .T.
    Local aDetalhe := {}
    Local aMovimento := {}
    Local cData := Substr(aItens[nLin,2],7,4) + "-" + Substr(aItens[nLin,2],4,2) + "-" + Substr(aItens[nLin,2],1,2)
    Local cDtBai
    
    U_M0601A(cData,aItens[nLin,3],@aDetalhe,@aMovimento)

    If Len(aDetalhe) > 0
        lEditCell(@aItens,oListBox,"@D",4)

        cDtBai := Substr(aItens[nLin,4],7,4) + Substr(aItens[nLin,4],4,2) + Substr(aItens[nLin,4],1,2)
        
        If cDtBai != aDetalhe[1,7]
            aItens[nLin,4] := aItens[nLin,2]
            oListBox:Refresh()
        EndIf
    EndIf

Return(lRet)
