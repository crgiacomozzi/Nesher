#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"
#INCLUDE "RestFul.ch"

/*/{Protheus.doc} M0603
    Função responsável pela baixa de títulos recebidos da API da Koncilli
    @type  Function
    @author Pono Tecnologia
    @since 21/06/2022
    @version 12.1.33
    /*/
User Function M0603(cCod,cCanal)
    Local aArea         := GetArea()
    Private aCpoInfo    := {}
    Private aCampos     := {}
    Private aCpoData    := {}
    Private oTable      := Nil
    Private oMarkBrow   := Nil
    Private aRotina     := MenuDef()
    Private aMovBan     := {}

    FwMsgRun(,{ || U_M0603A(cCod,cCanal) }, "Processamento de títulos da Koncili", 'Carregando dados...')

    oMarkBrow := FwMarkBrowse():New()
    oMarkBrow:SetAlias('TRB2')
    oMarkBrow:SetTemporary()

	//Legenda da grade, é obrigatório carregar antes de montar as colunas
	oMarkBrow:AddLegend("TMP_STATUS=='A'","GREEN" 	,"Título Aberto")
	oMarkBrow:AddLegend("TMP_STATUS=='B'","BLUE"  	,"Título Baixado sem liquidação")
	oMarkBrow:AddLegend("TMP_STATUS=='C'","YELLOW"  ,"Título liquidado sem baixa")
    oMarkBrow:AddLegend("TMP_STATUS=='D'","RED"  	,"Título Baixado e liquidado")

    oMarkBrow:SetColumns(aCampos)
    oMarkBrow:SetFieldMark('TMP_OK')
    oMarkBrow:SetMenuDef('M06003')
    oMarkBrow:SetDescription('Processamento de títulos Koncilli')
    oMarkBrow:SetAllMark( { || U_M0603J(oMarkBrow:Mark(),lMarcar := !lMarcar ), oMarkBrow:Refresh(.T.)  } )
    oMarkBrow:Activate()
    
    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    RestArea(aArea)

Return

User Function M0603A(cCod,cCanal)
    Local nI, nX
    Local aCarrega := {}

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    oTable   := FwTemporaryTable():New('TRB2')
    aCampos  := {}
    aCpoInfo := {}
    aCpoData := {}

    aAdd(aCpoInfo, {'Marcar'                , '@!' , 1})
    aAdd(aCpoInfo, {'Marketplace'           , '@!' , 1})
    aAdd(aCpoInfo, {'Id Conciliação'        , '@!' , 1})
    aAdd(aCpoInfo, {'Cliente'               , '@!' , 1})
    aAdd(aCpoInfo, {'Loja'                  , '@!' , 1})
    aAdd(aCpoInfo, {'Nome'                  , '@!' , 1})
    aAdd(aCpoInfo, {'Título'                , '@!' , 1})
    aAdd(aCpoInfo, {'Valor'                 , '@E 999,999,999.99' , 2})
    aAdd(aCpoInfo, {'Valor Pago'            , '@E 999,999,999.99' , 2})
    aAdd(aCpoInfo, {'Valor Comissão'        , '@E 999,999,999.99' , 2})
    aAdd(aCpoInfo, {'Juros'                 , '@E 999,999,999.99' , 2})
    aAdd(aCpoInfo, {'Data Pagamento'        , '@D' , 1})
    aAdd(aCpoInfo, {'Código Marketplace'    , '@!' , 1})
    aAdd(aCpoInfo, {'Status'                , '@!' , 1})
    aAdd(aCpoInfo, {'Baixado'               , '@!' , 1})
    aAdd(aCpoInfo, {'Liquidado'             , '@!' , 1})

    aAdd(aCpoData, {'TMP_OK'        , 'C' , 01, 0})
    aAdd(aCpoData, {'TMP_CANAL'     , 'C' , 10, 0})
    aAdd(aCpoData, {'TMP_IDCONC'    , 'C' , 06, 0})
    aAdd(aCpoData, {'TMP_CLIENT'    , 'C' , 06, 0})
    aAdd(aCpoData, {'TMP_LOJA'      , 'C' , 02, 0})
    aAdd(aCpoData, {'TMP_NOME'      , 'C' , 30, 0})
    aAdd(aCpoData, {'TMP_TITULO'    , 'C' , 15, 0})
    aAdd(aCpoData, {'TMP_VALOR'     , 'N' , 14, 2})
    aAdd(aCpoData, {'TMP_VALPAG'    , 'N' , 14, 2})
    aAdd(aCpoData, {'TMP_VALCOM'    , 'N' , 14, 2})
    aAdd(aCpoData, {'TMP_VALJUR'    , 'N' , 14, 2})
    aAdd(aCpoData, {'TMP_DTPGT'     , 'D' , 08, 0})
    aAdd(aCpoData, {'TMP_CODMAR'    , 'C' , 30, 0})
    aAdd(aCpoData, {'TMP_STATUS'    , 'C' , 01, 0})
    aAdd(aCpoData, {'TMP_BAIXA'     , 'C' , 01, 0})
    aAdd(aCpoData, {'TMP_LIQUID'    , 'C' , 01, 0})

    For nI := 1 To Len(aCpoData)

        If(aCpoData[nI][1] <> 'TMP_OK' .AND. aCpoData[nI][1] <> 'TMP_STATUS' .AND. aCpoData[nI][1] <> 'TMP_BAIXA' .AND. aCpoData[nI][1] <> 'TMP_LIQUID') 

            aAdd(aCampos, FwBrwColumn():New())

            aCampos[Len(aCampos)]:SetData( &('{||' + aCpoData[nI,1] + '}') )
            aCampos[Len(aCampos)]:SetTitle(aCpoInfo[nI,1])
            aCampos[Len(aCampos)]:SetPicture(aCpoInfo[nI,2])
            aCampos[Len(aCampos)]:SetSize(aCpoData[nI,3])
            aCampos[Len(aCampos)]:SetDecimal(aCpoData[nI,4])
            aCampos[Len(aCampos)]:SetAlign(aCpoInfo[nI,3])

        EndIf

    Next nI    

    oTable:SetFields(aCpoData)

    oTable:Create()

    aCarrega = U_M0603B(cCod,cCanal)

    If Len(aCarrega) > 0

        For nX := 1 To Len(aCarrega)

            DbSelectArea('TRB2')

            RecLock('TRB2', .T.)

                TRB2->TMP_CANAL    := cCanal
                TRB2->TMP_IDCONC   := cCod
                TRB2->TMP_CLIENT   := aCarrega[nX,1]
                TRB2->TMP_LOJA     := aCarrega[nX,2]
                TRB2->TMP_NOME     := aCarrega[nX,3]
                TRB2->TMP_TITULO   := aCarrega[nX,4]
                TRB2->TMP_VALOR    := aCarrega[nX,5]
                TRB2->TMP_VALPAG   := aCarrega[nX,6]
                TRB2->TMP_VALCOM   := aCarrega[nX,7]
                TRB2->TMP_VALJUR   := aCarrega[nX,8]
                TRB2->TMP_DTPGT    := STOD(aCarrega[nX,9])
                TRB2->TMP_CODMAR   := aCarrega[nX,10]
                TRB2->TMP_STATUS   := aCarrega[nX,11]
                TRB2->TMP_BAIXA    := aCarrega[nX,12]
                TRB2->TMP_LIQUID   := aCarrega[nX,13]

            TRB2->(MsUnlock())
        
        Next nX

        TRB2->(DbGoTop())

    EndIf

Return

Static Function MenuDef()
    Local aRot := {}

    ADD Option aRot Title 'Processar'    Action 'FwMsgRun(,{ || U_M0603G(), CloseBrowse() }, "Processando dos títulos selecionados", "Aguarda...")' Operation 1 Access 0
    ADD Option aRot Title 'Visualizar'   Action 'FwMsgRun(,{ || U_M0603N(TRB2->TMP_CANAL,TRB2->TMP_CODMAR) }, "Processando dos títulos selecionados", "Aguarda...")' Operation 2 Access 0
    ADD Option aRot Title 'Cancelamento' Action 'FwMsgRun(,{ || U_M0603X(), CloseBrowse() }, "Processando cancelamento e liquidaçã dos títulos selecionados", "Aguarda...")' Operation 1 Access 0

Return(aRot)

User Function M0603B(cCod,cCanal)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX, nY, nZ
    Local aBaixa    := {}
    Local aTitulo   := {}
    Local dDtIni    := Substr(DTOS(MV_PAR01),1,4) + "-" + Substr(DTOS(MV_PAR01),5,2) + "-" + Substr(DTOS(MV_PAR01),7,2)
    Local dDtFim    := Substr(DTOS(MV_PAR02),1,4) + "-" + Substr(DTOS(MV_PAR02),5,2) + "-" + Substr(DTOS(MV_PAR02),7,2)
    Local nLimit    := 1
    Local nOffSet   := 1
    Local nCount    := 1
    Local nVezes    := 1
    Local cTipo
    Local nValPag   := 0.00
    Local nValLiq   := 0.00
    Local nValJur   := 0.00
    Local lBaixado  := .F.
    Local lLiquida  := .F.
    Local cStatus   := ""

    aMovBan := {}

    If !Empty(Alltrim(DTOS(MV_PAR01))) .AND. !Empty(Alltrim(DTOS(MV_PAR02)))

        U_M0602C(@nLimit,@nOffSet,@nCount,@nVezes,@cCod,@cCanal)

        For nY := 1 to nVezes
            cUrl  := "https://api-sandbox.koncili.com"
            cPath := "/externalapi/orderextract/unresolveds?offset="+ Alltrim(Str(nOffSet)) + "&initDate=" + dDtIni + "&endDate=" + dDtFim
            If !Empty(Alltrim(cCod))
                cPath += "&conciliationId=" + Alltrim(cCod) + "&channelName=" + Alltrim(cCanal)
            EndIf
            cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

            Aadd(aHeader,'Accept: application/json')
            Aadd(aHeader,'Content-Type: application/json')
            AADD(aHeader, "gumgaToken: " + cAuth)

            oRest := FWRest():new(cUrl)
            oRest:setPath(cPath)
            oJson := JsonObject():new()

            If oRest:Get(aHeader)
                If ValType(oRest:ORESPONSEH) == "O"
                    cJson := oRest:GetResult()
                    cJson := StrTran(cJson,"ï»¿","")
                    cParser  := oJson:FromJson(cJson)
                EndIf
                If Empty(cParser)
                    For nX := 1 To Len(oJson['content'])
                        cOrder  := oJson['content'][nX]['orderCode']
                        cData   := StrTran(SubStr(oJson['content'][nX]['conciliationEndDate'],1,10),"-","")
                        cTipo   := oJson['content'][nX]['extractType']
                        nValor  := oJson['content'][nX]['releasedValue']
                        If nValor < 0 
                            nValor := nValor * (-1)
                        EndIf
                        aTitulo := U_M0603C(cOrder)
                        If Len(aTitulo) > 0
                            nValPag := 0.00
                            nValLiq := 0.00
                            nValJur := 0.00
                            nPosField := 0
                            nPosField := aScan( aBaixa, {|x| AllTrim(x[10]) == Alltrim(cOrder) } )
                            If nPosField == 0
                                AADD( aBaixa, { aTitulo[1,1],aTitulo[1,2],aTitulo[1,3],aTitulo[1,4],aTitulo[1,5],nValPag,nValLiq,nValJur,cData,cOrder,cStatus, .T., .T. } )
                            EndIf
                        EndIf
                    Next nX
                EndIf
            EndIf
            nOffSet += nLimit
        Next nY

        freeObj(oJson)

        For nZ := 1 To Len(aBaixa)
            nValPag := 0.00
            nValLiq := 0.00
            nValJur := 0.00
            U_M0603D(aBaixa[nZ,10],cCanal,@nValPag,@nValLiq,@nValJur)
            aBaixa[nZ,6] += nValPag
            aBaixa[nZ,7] += nValLiq
            aBaixa[nZ,8] += nValJur
            lBaixado := U_M0603K(aBaixa[nZ,10],MV_PAR03)
            lLiquida := U_M0603L(aBaixa[nZ,1],aBaixa[nZ,2],aBaixa[nZ,4])
            cStatus := U_M0603M(lBaixado,lLiquida)
            aBaixa[nZ,11] := cStatus
            aBaixa[nZ,12] := Iif(lBaixado,"S","N")
            aBaixa[nZ,13] := Iif(lLiquida,"S","N")
        Next nZ
    EndIf

Return(aBaixa)

User Function M0603C(cWareId)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local aRet   := {}

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '02'
    cQuery += "AND E1_IDWARE = '" + Alltrim(cWareId) + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty((cAlias)->E1_IDWARE)
        AADD( aRet, { (cAlias)->E1_CLIENTE, (cAlias)->E1_LOJA, (cAlias)->E1_NOMCLI, (cAlias)->E1_NUM, (cAlias)->E1_VALOR } )
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(aRet)

User Function M0603D(cOrder,cCanal,nValPag,nValLiq,nValJur)
    Local aOcoKon := {}
    Local aOcoPro := {}
    Local nX, nY

    U_M0603E(cOrder,cCanal,@aOcoKon)

    If Len(aOcoKon) > 0
        For nX := 1 To Len(aOcoKon)
            aOcoPro := {}
            U_M0603F(cCanal,aOcoKon[nX,1],@aOcoPro)
            For nY := 1 to Len(aOcoPro)
                If aOcoPro[nY,1] == "B"
                    If aOcoPro[nY,4] == "P"
                        If aOcoPro[nY,3] == "S"
                            nValPag += aOcoKon[nX,2]
                        ElseIf aOcoPro[nY,3] == "D"
                            nValPag -= aOcoKon[nX,2]
                        EndIf
                    ElseIf aOcoPro[nY,4] == "C"
                        If aOcoPro[nY,3] == "S"
                            nValLiq += aOcoKon[nX,2]
                        ElseIf aOcoPro[nY,3] == "D"
                            nValLiq += aOcoKon[nX,2]
                        EndIf
                    ElseIf aOcoPro[nY,4] == "J"
                        If aOcoPro[nY,3] == "S"
                            nValJur += aOcoKon[nX,2]
                        ElseIf aOcoPro[nY,3] == "D"
                            nValJur -= aOcoKon[nX,2]
                        EndIf
                    EndIf
                ElseIf aOcoPro[nY,1] == "L"
                    If aOcoPro[nY,4] == "P"
                        If aOcoPro[nY,3] == "S"
                            nValPag += aOcoKon[nX,2]
                        ElseIf aOcoPro[nY,3] == "D"
                            nValPag -= aOcoKon[nX,2]
                        EndIf
                    ElseIf aOcoPro[nY,4] == "C"
                        If aOcoPro[nY,3] == "S"
                            nValLiq += aOcoKon[nX,2]
                        ElseIf aOcoPro[nY,3] == "D"
                            nValLiq -= aOcoKon[nX,2]
                        EndIf
                    ElseIf aOcoPro[nY,4] == "J"
                        If aOcoPro[nY,3] == "S"
                            nValJur += aOcoKon[nX,2]
                        ElseIf aOcoPro[nY,3] == "D"
                            nValJur -= aOcoKon[nX,2]
                        EndIf
                    EndIf
                ElseIf aOcoPro[nX,1] == "M"
                    AADD(aMovBan, { aOcoKon[nX,2], aOcoPro[nX,5], aOcoPro[nX,6], aOcoPro[nX,7], aOcoPro[nX,8], cCanal } )
                EndIf
            Next nY
        Next nX
    EndIf

Return

User Function M0603E(cOrder,cCanal,aOcoKon)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX

    cUrl  := "https://api-sandbox.koncili.com"
    cPath := "/externalapi/orderextract/"+ Alltrim(cOrder) + "/" + Alltrim(cCanal)
    cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

    Aadd(aHeader,'Accept: application/json')
    Aadd(aHeader,'Content-Type: application/json')
    AADD(aHeader, "gumgaToken: " + cAuth)

    oRest := FWRest():new(cUrl)
    oRest:setPath(cPath)
    oJson := JsonObject():new()

    If oRest:Get(aHeader)
        If ValType(oRest:ORESPONSEH) == "O"
            cJson := oRest:GetResult()
            cJson := StrTran(cJson,"ï»¿","")
            cParser  := oJson:FromJson(cJson)
        EndIf
        If Empty(cParser)
            For nX := 1 To Len(oJson['elements'])
                cOrder  := oJson['elements'][nX]['orderCode']
                cTipo   := oJson['elements'][nX]['extractType']
                nValor  := oJson['elements'][nX]['releasedValue']
                If nValor < 0
                    nValor := nValor * (-1)
                EndIf
                AADD( aOcoKon, { cTipo,nValor } )
            Next nX
        EndIf
    EndIF

    freeObj(oJson)

Return

User Function M0603F(cCanal,cTipo,aOcoPro)
    Local cAlias := GetNextAlias()
    Local cQuery

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("ZZB") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND ZZB_FILIAL = '" + xFilial("ZZB") + "' "
    cQuery += "AND ZZB_MKTID = '" + Alltrim(cCanal) + "'
    cQuery += "AND TRIM(ZZB_DESCRI) = '" + Alltrim(cTipo) + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !(cAlias)->((Eof()))
        AADD( aOcoPro, { (cAlias)->ZZB_TIPO, (cAlias)->ZZB_DESCRI, (cAlias)->ZZB_OPERA, (cAlias)->ZZB_CAMPO, (cAlias)->ZZB_NATURE, (cAlias)->ZZB_HISTOR, (cAlias)->ZZB_TPMOVB, (cAlias)->ZZB_IDMOVB } )
        (cAlias)->(dbSkip())
    EndDo
    
    (cAlias)->(dbCloseArea())

Return

User Function M0603G()
    Local aArea    := GetArea()
    Local cPrefixo := "1  "
    Local cPreLiq  := "CCA"
    Local cParcela := " "
    Local cTpTit   := "NF "
    Local lBaixado := .F.
    Local lLiquida := .F.
    Local lMovime  := .F.
    Local cCod
    Local cCanal
    Local aImprime := {}
    Local aErros   := {}
    Local cMsg
    Local nX
    Local nOpcao   := 3

    TRB2->(dbGoTop())

    cCod    := Alltrim(TRB2->TMP_IDCONC)
    cCanal  := Alltrim(TRB2->TMP_CANAL)

    If MV_PAR03 != dDataBase
        Help(,,"Processamento de títulos Koncili",,"A data basa de sistema deve ser a mesma que a data da baixa dos títulos.",1,0,,,,,,{"Altere a data basa do sistema."})
        Return
    EndIf

    While !TRB2->(Eof())
        Begin Transaction
            If !Empty(TRB2->TMP_OK)
                If TRB2->TMP_LIQUID == "N" .AND. TRB2->TMP_VALCOM > 0
                    lLiquida := U_M0603H(TRB2->TMP_CLIENT,TRB2->TMP_LOJA,TRB2->TMP_TITULO,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,cPrefixo,cPreLiq,cTpTit,TRB2->TMP_CODMAR,cMsg,nOpcao)
                    If !lLiquida
                        AADD( aErros, { DTOC(TRB2->TMP_DTPGT),TRB2->TMP_CANAL,cMsg } )
                    EndIf
                EndIf
                If TRB2->TMP_BAIXA == "N" .AND. TRB2->TMP_VALPAG > 0
                    lBaixado := U_M0603I(TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cParcela,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALJUR,TRB2->TMP_CODMAR,cMsg,nOpcao)
                    If !lBaixado
                        AADD( aErros, { DTOC(TRB2->TMP_DTPGT),TRB2->TMP_CANAL,cMsg } )
                    EndIf
                EndIf
            EndIf
        If lLiquida .AND. !lBaixado
            AADD( aImprime, { TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,DTOC(TRB2->TMP_DTPGT),"Título Liquidado" } )
        ElseIf !lLiquida .AND. lBaixado
            AADD( aImprime, { TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,DTOC(TRB2->TMP_DTPGT),"Título Baixado" } )
        ElseIf lLiquida .AND. lBaixado
            AADD( aImprime, { TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,DTOC(TRB2->TMP_DTPGT),"Título Baixado e Liquidado" } )
        EndIf
        lLiquida := .F.
        lBaixado := .F.
        TRB2->(dbSkip())
        End Transaction
    EndDo

    If Len(aMovBan) > 0
        For nX := 1 To Len(aMovBan)
            Begin Transaction
                lMovime := U_M0603V(aMovBan[nX],cMsg)
                    If !lMovimen
                        AADD( aErros, { DTOC(ddatabase),aMovBan[nY,6],cMsg } )
                    EndIf
            End Transaction
        Next nX
    EndIf

    If Len(aImprime) > 0
        U_M0603T(aImprime)
    EndIf

    If Len(aErros)
        U_M0603U(aErros)
    EndIf

    RestArea(aArea)

Return

/*/{Protheus.doc} M0601H
    Função que realiza o MSExecAuto da liquidação do título
    @type  Function
    @author Pono
    @since 01/07/2022
    @version 12.1.33
    @param cFilLiq  , Character, filial do título
    @param cCliLiq  , Character, código do cliente
    @param cLojLiq  , Character, loja do cliente
    @param cPreLiq  , Character, prefixo do título
    @param cNumLiq  , Character, número do título
    @param cParLiq  , Character, parcela do título
    @param cTipLiq  , Character, tipo do título
    @param nValPago , Character, valor da baixa
    @param nValLiq  , Character, valor da comissão (valor do título de liquidação)
    @param cMsg     , Character, mensagem de resultado do processamento do MSExecAuto
    @return Logical, retorna resultado do MSExcAuto de liquidação.
    /*/
User Function M0603H(cCliLiq,cLojLiq,cNumLiq,nValPago,nValLiq,cPrefixo,cPreLiq,cTipLiq,cCodMkp,cMsg,nOpcao)
    Local aArea             := GetArea()
    Local aCab              := {}
    Local aTit              := {}
    Local aItens            := {}
    Local cFiltro           := ""
    Local lRet              := .T.
    Local cCond             := "300"
    Local aBanco	        := Separa( GetNewPar("MV_YECOBCO","001/0021 /84000     "),"/")
    Local cNatureza         := '020100'
    Local nMoeda            := 1
    Local nI
    Local aErro             := {}
    Local cCanLiq           := ""
    Private lMsErroAuto     := .F.
    Private lAutoErrNoFile  := .T. 

    If nOpcao == 3
        dbSelectArea("SA6")
        SA6->(dbGotop())
        SA6->(dbSetOrder(1))
        SA6->(dbSeek(xFilial("SA6")+aBanco[1]+aBanco[2]+aBanco[3]))

        cFiltro := "E1_FILIAL == " + valtosql(xFilial("SE1")) + " .AND. "
        cFiltro += "E1_PREFIXO == " + valtosql(cPrefixo) + " .AND. "
        cFiltro += "E1_NUM == " + valtosql(Alltrim(cNumLiq)) + " .AND. "
        cFiltro += "E1_TIPO == " + valtosql(cTipLiq) + " .AND. "
        cFiltro += "E1_CLIENTE == " + valtosql(cCliLiq) + " .AND. "
        cFiltro += "E1_LOJA == " + valtosql(cLojLiq) + " .AND. "
        cFiltro += "E1_NUMLIQ == " 	+ ValToSql( CriaVar("E1_NUMLIQ",.F.) ) + " .AND. "
        cFiltro += "E1_SITUACA $ '0FG' .AND. E1_SALDO > 0"

        aAdd( aCab, {"cCondicao" , cCond     })
        aAdd( aCab, {"cNatureza" , cNatureza })
        aAdd( aCab, {"E1_TIPO"   , cTipLiq   })
        aAdd( aCab, {"cCLIENTE"  , cCliLiq   })
        aAdd( aCab, {"nMoeda"    , nMoeda    })
        aAdd( aCab, {"cLOJA"     , cLojLiq   })

        Aadd(aTit, {"E1_PREFIXO"	, cPreLiq			})
        Aadd(aTit, {"E1_NUM" 		, Alltrim(cNumLiq)	})
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

        aAdd( aItens, aTit )

        msExecAuto( { |a,b,c,d,e| FINA460(a,b,c,d,e) }, Nil, aCab, aItens, nOpcao, cFiltro )
        If lMsErroAuto
            lRet := .F.
            MostraErro()
            cMsg := "Título : " + cNumLiq + CRLF
            cMsg += "Cod. Marketplace : " + cCodMkp + CRLF
            aErro := GetAutoGRLog()
            For nI := 1 To Len(aErro)
                cMsg += aErro[nI] + CRLF
            Next nI
        EndIf

        SA6->(dbCloseArea())

    ElseIf nOpcao == 5
        dbSelectArea("SE1")
        SE1->(dbGotop())
        SE1->(dbSetOrder(2))
        If SE1->(dbSeek(xFilial("SE1")+cCliLiq+cLojLiq+cPreLiq+Alltrim(cNumLiq)+"L"+cTipLiq))
            cCanLiq := SE1->E1_NUMLIQ
        Else
            lRet := .F.
            cMsg := "Título : " + cNumLiq + CRLF
            cMsg += "Cod. Marketplace : " + cCodMkp + CRLF
            cMsg += "Título de liquidação não encontrado para exclusão."
        EndIf

        If !Empty(Alltrim(cCanLiq))
            msExecAuto( { |a,b,c,d,e,f| FINA460(a,b,c,d,e) }, Nil, Nil, Nil, nOpcao, Nil, cCanLiq )
            If lMsErroAuto
                lRet := .F.
                MostraErro()
                cMsg := "Título : " + cNumLiq + CRLF
                cMsg += "Cod. Marketplace : " + cCodMkp + CRLF
                aErro := GetAutoGRLog()
                For nI := 1 To Len(aErro)
                    cMsg += aErro[nI] + CRLF
                Next nI
            EndIf
        EndIf

        SE1->(dbCloseArea())

    EndIf

    RestArea(aArea)

Return(lRet)

/*/{Protheus.doc} M0603I
    Função que realiza o MSExecAuto da baixa do título
    @type  Function
    @author Pono
    @since 01/07/2022
    @version 12.1.23
    @param cPrefixo , Character, prefixo do título
    @param cNumero  , Character, número do título
    @param cParcela , Character, parcela do título
    @param cTipo    , Character, tipo do título
    @param nValPago , Numeric  , valor da baixa
    @param cDtPgto  , Character, data da baixa
    @param cMsg     , Character, mensagem caso falhe o msexecauto da baixa
    @param cPedPrec , Character, código do pedido do Precode
    @param nJuros   , Numeric  , valores reembolsados pelo marketplace
    @return Logical, retorna false caso título já tenha sido baixado anteriormente.
    /*/
User Function M0603I(cCliente,cLoja,cPrefixo,cNumero,cParcela,cTipo,nValPago,nJuros,cCodMkp,cMsg,nOpcao)
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

    Aadd(aTitBaixa, {"E1_PREFIXO"  , cPrefixo         , nil})
    Aadd(aTitBaixa, {"E1_NUM"      , Alltrim(cNumero) , nil})
    Aadd(aTitBaixa, {"E1_PARCELA"  , cParcela         , nil})
    Aadd(aTitBaixa, {"E1_TIPO"     , cTipo            , nil})
    Aadd(aTitBaixa, {"E1_CLIENTE"  , cCliente         , nil})
    Aadd(aTitBaixa, {"E1_LOJA"     , cLoja            , nil})
    Aadd(aTitBaixa, {"AUTBANCO"    , SA6->A6_COD      , nil})
    Aadd(aTitBaixa, {"AUTAGENCIA"  , SA6->A6_AGENCIA  , nil})
    Aadd(aTitBaixa, {"AUTCONTA"    , SA6->A6_NUMCON   , nil})
    Aadd(aTitBaixa, {"AUTJUROS"    , 0                , nil,.T.})
    Aadd(aTitBaixa, {"AUTMULTA"    , ABS(nJuros)      , nil})
    Aadd(aTitBaixa, {"AUTVALREC"   , ABS(nValPago)    , nil})
    Aadd(aTitBaixa, {"AUTMOTBX"    , cMotBai          , nil})
    Aadd(aTitBaixa, {"AUTDTBAIXA"  , ddatabase        , nil})
    Aadd(aTitBaixa, {"AUTDTCREDITO", ddatabase        , nil})
    Aadd(aTitBaixa, {"AUTHIST"     , cHist            , nil})

    MSExecAuto({|x,y| Fina070(x,y)},aTitBaixa,nOpcao)
    If lMsErroAuto
        lBaixa := .F.
        cMsg := "Título : " + cNumero + CRLF
        cMsg += "Cod. Marketplace : " + cCodMkp + CRLF
        aErro := GetAutoGRLog()
        For nI := 1 To Len(aErro)
            cMsg += aErro[nI] + CRLF
        Next nI
    Else
        If nOpcao == 3
            dbSelectArea("SE1")
            SE1->(dbSetOrder(1))
            If (SE1->(dbSeek("02"+cPrefixo+Alltrim(cNumero)+cParcela+cTipo)))
                cCodPed := SE1->E1_YPEDPRE
                dbSelectArea("SE5")
                SE5->(dbSetOrder())
                If (SE5->(dbSeek("02")+cPrefixo+Alltrim(cNumero)+cParcela+cTipo))
                    RecLock("SE5",.F.)
                    //SE5->E5_IDWARE  := cCodMkp
                    SE5->E5_YIDPREC := cCodPed
                    SE5->(MsUnlock())
                EndIf
                SE5->(dbCloseArea())
            EndIf
            SE1->(dbCloseArea())
        EndIf
    EndIf

    SA6->(dbCloseArea())

    RestArea(aArea)

Return(lBaixa)

User Function M0603J(cMarca,lMarcar)
    Local cAlias := 'TRB2'
    Local lRet   := .T.
 
    dbSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    While !(cAlias)->( Eof() )
        RecLock((cAlias),.F.)
        (cAlias)->TMP_OK := IIf( lMarcar, cMarca, '  ' )
        MsUnlock()
        (cAlias)->( dbSkip() )
    EndDo

Return(lRet)

/*/{Protheus.doc} M0603K
    Função que verifica se o título já foi baixado anteriormente.
    @type  Function
    @author Pono
    @since 04/07/2022
    @version 12.1.33
    @param cWareId, Character, código do título no marketplace
    @return Logical, retorna false caso título já tenha sido baixado anteriormente.
    /*/
User Function M0603K(cWareId,dData)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local lRet   := .F.

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE1") + " E1 "
    cQuery += "WHERE E1.D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '02' "
    cQuery += "AND E1_IDWARE = '" + Alltrim(cWareId) + "'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If (cAlias)->E1_SALDO == 0
        lRet := .T.
    ElseIf (cAlias)->E1_SALDO > 0 .AND. (cAlias)->E1_SALDO <> (cAlias)->E1_VALOR
        lRet := U_M0603W((cAlias)->E1_PREFIXO, (cAlias)->E1_NUM, (cAlias)->E1_PARCELA, (cAlias)->E1_CLIFOR, (cAlias)->E1_LOJA,dData)
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(lRet)

/*/{Protheus.doc} M0603L
    Função que verifica se o título já foi liquidado anteriormente.
    @type  Function
    @author Pono
    @since 04/07/2022
    @version 12.1.33
    @param cWareId, Character, código do título no marketplace
    @return Logical, retorna false caso título já tenha sido liquidado anteriormente.
    /*/
User Function M0603L(cCliente,cLoja,cNumero)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local lRet   := .F.
    
    cQuery := "SELECT COUNT(*) AS LIQUIDA "
    cQuery += "FROM " + RetSqlName("SE1") + " E1 "
    cQuery += "WHERE E1.D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '02' "
    cQuery += "AND E1_CLIENTE = '" + cCliente + "' "
    cQuery += "AND E1_LOJA = '" + cLoja + "' "
    cQuery += "AND (E1_PREFIXO = 'CCA' OR E1_PREFIXO = '1') "
    cQuery += "AND E1_NUM = '" + cNumero + "' "
    cQuery += "AND E1_TIPO = 'NF' "
    cQuery += "AND E1_NUMLIQ != ''"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If (cAlias)->LIQUIDA > 0
        lRet := .T.
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(lRet)

User Function M0603M(lBaixado,lLiquida)
    Local cStatus := ""

    If lBaixado .AND. lLiquida
        cStatus := "D"
    ElseIf lBaixado .AND. !lLiquida
        cStatus := "B"
    ElseIf !lBaixado .AND. lLiquida
        cStatus := "C"
    ElseIf !lBaixado .AND. !lLiquida
        cStatus := "A"
    EndIf

Return(cStatus)

User Function M0603N(cCanal,cOrder)
    Local lRet      := .T.
    Local aFields   := {" ", "Canal" , "Código do título","Tipo          ", "Descrição do Tipo", "Valor", "Situação"}
    Local aButtons  := {}
    Local aItens    := {}

        U_M0603O(cCanal,cOrder,@aItens)

        If Len(aItens) > 0
            oDlg := FWDialogModal():New()

            oDlg:SetEscClose(.T.)
            oDlg:SetTitle("Conciliação de títulos")
            
            //Seta a largura e altura da janela em pixel
            oDlg:setSize(250, 550)

            oDlg:CreateDialog()
            oContainer := TPanel():New( ,,, oDlg:getPanelMain() )
            oContainer:Align := CONTROL_ALIGN_ALLCLIENT

            cLine := "{U_M0603P(aItens[oListBox:nAT][2],aItens[oListBox:nAT][4]),aItens[oListBox:nAT][2],aItens[oListBox:nAT][3],aItens[oListBox:nAT][4],aItens[oListBox:nAT][5],aItens[oListBox:nAT][6],aItens[oListBox:nAT][7] } "
            bLine := &( "{ || " + cLine + " }" )

            oListBox:=TWBrowse():New( 001,001,550,200,,aFields,,oContainer,,,,,,,,,,,,.F.,,.T.,,.F.,,,)
            oListBox:SetArray(aItens)
            oListBox:bLDblClick := { || aItens[oListBox:nAt,1] := !aItens[oListBox:nAt,1] }
            oListBox:bLine := bLine
            oListBox:bLDblClick := { || U_M0603S() }

            oDlg:AddButton( 'Fechar'	,{|| oDlg:DeActivate() }, 'Fechar' , , .T., .F., .T., )

            oDlg:addButtons(aButtons)

            oDlg:Activate()
        Else
            lRet := .F.
            Help(,, "Conciliação de Títulos Koncili",, "Não existem registros para o título seleciondo !!!", 1, 0,,,,,, {"Selecione outro título."})
        EndIf

Return

User Function M0603O(cCanal,cOrder,aItens)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX
    Local aOcorre   := {}

    U_M0603R(cCanal,@aOcorre)
    
    cUrl  := "https://api-sandbox.koncili.com"
    cPath := "/externalapi/orderextract/"+ Alltrim(cOrder) + "/" + Alltrim(cCanal)
    cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

    Aadd(aHeader,'Accept: application/json')
    Aadd(aHeader,'Content-Type: application/json')
    AADD(aHeader, "gumgaToken: " + cAuth)

    oRest := FWRest():new(cUrl)
    oRest:setPath(cPath)
    oJson := JsonObject():new()

    If oRest:Get(aHeader)
        If ValType(oRest:ORESPONSEH) == "O"
            cJson := oRest:GetResult()
            cJson := StrTran(cJson,"ï»¿","")
            cParser  := oJson:FromJson(cJson)
        EndIf
        If Empty(cParser)
            For nX := 1 To Len(oJson['elements'])
                cOrder    := oJson['elements'][nX]['orderCode']
                cTipo     := oJson['elements'][nX]['extractType']
                nValor    := oJson['elements'][nX]['releasedValue']
                cSituacao := oJson['elements'][nX]['situation']
                If nValor < 0
                    nValor := nValor * (-1)
                EndIf
                cTpTrad := ""
                If Len(aOcorre)
                    nPosField = 0
                    nPosField := aScan( aOcorre, {|x| AllTrim(x[1]) == Alltrim(cTipo) } )
                    cTpTrad := aOcorre[nPosField,2]
                EndIf

                AADD( aItens, { .F., cCanal, cOrder, cTipo, cTpTrad, Transform(nValor,'@E 999,999,999.99') , cSituacao } )
            Next nX
        EndIf
    EndIF

    freeObj(oJson)

Return

User Function M0603P(cCanal,cTipo)
    Local oOK       := LoadBitmap(GetResources(),'BR_VERDE')
    Local oNO       := LoadBitmap(GetResources(),'BR_VERMELHO')
    Local oCor
    Local lAchou    := .F.

    lAchou := U_M0603Q(cCanal,cTipo)
    
    If lAchou
        oCor := oOK
    Else
        oCor := oNO
    EndIf

Return(oCor)

User Function M0603Q(cCanal,cTipo)
    Local lRet := .F.
    Local cAlias := GetNextAlias()
    Local cQuery

    cQuery := "SELECT ZZB_MKTID "
    cQuery += "FROM " + RetSqlName("ZZB") + " ZZB "
    cQuery += "WHERE ZZB.D_E_L_E_T_ = ' ' "
    cQuery += "AND ZZB_FILIAL = '" + xFilial("ZZB") + "'
    cQuery += "AND TRIM(ZZB_MKTID) = '" + Alltrim(cCanal) + "'
    cQuery += "AND TRIM(ZZB_DESCRI) = '" + Alltrim(cTipo) + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty(Alltrim((cAlias)->ZZB_MKTID))
        lRet := .T.
    EndIf
    
    (cAlias)->(dbCloseArea())


Return(lRet)

User Function M0603R(cCanal,aOcorre)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX

    cUrl  := "https://api-sandbox.koncili.com"
    cPath := "/externalapi/releasetype/"+ Alltrim(cCanal)
    cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

    Aadd(aHeader,'Accept: application/json')
    Aadd(aHeader,'Content-Type: application/json')
    AADD(aHeader, "gumgaToken: " + cAuth)

    oRest := FWRest():new(cUrl)
    oRest:setPath(cPath)
    oJson := JsonObject():new()

    If oRest:Get(aHeader)
        If ValType(oRest:ORESPONSEH) == "O"
            cJson := oRest:GetResult()
            cJson := StrTran(cJson,"ï»¿","")
            cParser  := oJson:FromJson(cJson)
        EndIf
        If Empty(cParser)
            For nX := 1 To Len(oJson['elements'])
                cCode     := Alltrim(oJson['elements'][nX]['code'])
                cDesc     := DecodeUTF8(Alltrim(oJson['elements'][nX]['description']))
                AADD( aOcorre, { cCode, cDesc } )
            Next nX
        EndIf
    EndIF

    freeObj(oJson)

Return

User Function M0603S()
    Local aLegenda := {}
     
    aAdd(aLegenda,{"BR_VERDE",      "Ocorrência cadastrada"})
    aAdd(aLegenda,{"BR_VERMELHO",   "Ocorrência não cadastrada"})

    BrwLegenda("Cadastro de Ocorrências", "Status", aLegenda)

Return

/*/{Protheus.doc} M0603T
    Função para execução do relatórios do processamento da baixa dos títulos
    @type  Function
    @author Pono
    @since 08/07/2022
    @version 12.1.33
    @param aImprime, Array, array com os títulos processados (baixados ou liquidados) do ciclo.
    /*/
User Function M0603T(aImprime)
	Local oReport

    oReport := M0603TA(aImprime)
    oReport:PrintDialog()	

Return

/*/{Protheus.doc} M0603TA
    Função para execução do relatórios do processamento da baixa dos títulos
    @type  Function
    @author Pono
    @since 08/07/2022
    @version 12.1.33
    @param aImprime, Array, array com os títulos processados (baixados ou liquidados) do ciclo.
    /*/
Static Function M0603TA(aImprime)
	Local oReport
	Local oSection1

	oReport := TReport():New("BAIXAS","Relatório de títulos baixados","BAIXAS",{|oReport| M0603TB(oReport,aImprime)},"Relatório de títulos baixados")

	oReport:nFontBody := 9
	oReport:SetLandScape(.T.)

	oSection1 := TRSection():New(oReport,"Baixas",)

	TRCell():New(oSection1,"CODCLI"	    ," ","Cód.Cliente",,15)
	TRCell():New(oSection1,"LOJCLI"  	," ","Loja Cliente",,15)
	TRCell():New(oSection1,"PREFIXO"  	," ","Prefixo",,10)
	TRCell():New(oSection1,"NUMERO"  	," ","Nº Título",,20)
	TRCell():New(oSection1,"TIPO"     	," ","Tipo",,10)
    TRCell():New(oSection1,"VALPAG"	    ," ","Vlr Pago","@E 999,999,999.99",15)
    TRCell():New(oSection1,"VALCOM"	    ," ","Vlr Comissão","@E 999,999,999.99",15)
	TRCell():New(oSection1,"DATA"     	," ","Data Pagamento",,20)
    TRCell():New(oSection1,"TPBAIXA"   	," ","Tipo da Baixa",,35)

Return oReport

/*/{Protheus.doc} M0603TB
    Função para execução do relatórios do processamento da baixa dos títulos
    @type  Function
    @author Pono
    @since 08/07/2022
    @version 12.1.33
    @param aImprime, Array, array com os títulos processados (baixados ou liquidados) do ciclo.
    /*/
Static Function M0603TB(oReport,aImprime)
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

/*/{Protheus.doc} M0603U
    Função que exibe log de erros quando rotina de baixa de títulos Precode é executada a partir do menu
    @type  Function
    @author Pono
    @since 08/07/2022
    @version 12.1.33
    @param aErros, array, array contendo as mensagens dos erros encontrados durante o processamento
    /*/
User Function M0603U(aErros)
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

/*/{Protheus.doc} M0603V
    Função responsável pela inclusão de movimento bancário via MsExecAuto
    @type  Function
    @author Pono
    @since 12/07/2022
    @version 12.1.33
    @param aMovBan , Array    , array com as informações do movimentos bancários.
    @param cMsg    , Character, variável que recebe a mensagem de retorno do processamento do MsExecAuto.
    @return Logical, retorna um valor lógico indicando se o processamento ocorreu sem erros ou não.
    /*/
User Function M0603V(aMovBan,cMsg)
    Local aArea             := GetArea()
    Local aFina100          := {}
    Local lMovime           := .T.
    Local aBanco	        := Separa( GetNewPar("MV_YECOBCO","001/0021 /84000     "),"/")
    Local cMoeda            := "M1"
    Local nOpcao            := 3
    Local nI
    Private lMsErroAuto	    := .F.
    Private lAutoErrNoFile  := .T. 

    If aMovBan[2] == "R"
        nOpcao := 4
    EndIf

    cTitulo := Alltrim(aMovBan[5]) + DTOS(dDataBase) + Alltrim(aMovBan[6])

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
                    {"E5_NATUREZ"     ,Alltrim(aMovBan[2])      ,Nil},;
                    {"E5_BANCO"       ,SA6->A6_COD              ,Nil},;
                    {"E5_AGENCIA"     ,SA6->A6_AGENCIA          ,Nil},;
                    {"E5_CONTA"       ,SA6->A6_NUMCON           ,Nil},;
                    {"E5_VENCTO"      ,dDataBase                ,Nil},;
                    {"E5_BENEF"       ,"E-COMMERCE"             ,Nil},;
                    {"E5_HISTOR"      ,Alltrim(aMovBan[3])      ,Nil},;
                    {"E5_YIDPREC"     ,cTitulo                  ,Nil},;
                    {"NCTBONLINE"     ,2                        ,Nil}}     //1=Sim;2=Não
        
        msExecAuto({|x,y,z| Fina100(x,y,z)},0,aFina100,nOpcao)

        If lMsErroAuto
            cMsg := "Título : " + cTitulo + CRLF
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

User Function M0603X()
    Local aArea    := GetArea()
    Local cPrefixo := "1  "
    Local cPreLiq  := "CCA"
    Local cParcela := " "
    Local cTpTit   := "NF "
    Local lBaixado := .F.
    Local lLiquida := .F.
    Local cCod
    Local cCanal
    Local aImprime := {}
    Local aErros   := {}
    Local cMsg
    Local nOpcao   := 5

    TRB2->(dbGoTop())

    cCod    := Alltrim(TRB2->TMP_IDCONC)
    cCanal  := Alltrim(TRB2->TMP_CANAL)

    If MV_PAR03 != dDataBase
        Help(,,"Processamento de títulos Koncili",,"A data basa de sistema deve ser a mesma que a data da baixa dos títulos.",1,0,,,,,,{"Altere a data basa do sistema."})
        Return
    EndIf

    While !TRB2->(Eof())
        Begin Transaction
            If !Empty(TRB2->TMP_OK)
                If TRB2->TMP_LIQUID == "S"
                    lLiquida := U_M0603H(TRB2->TMP_CLIENT,TRB2->TMP_LOJA,TRB2->TMP_TITULO,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,cPrefixo,cPreLiq,cTpTit,TRB2->TMP_CODMAR,cMsg,nOpcao)
                    If !lLiquida
                        AADD( aErros, { DTOC(TRB2->TMP_DTPGT),TRB2->TMP_CANAL,cMsg } )
                    EndIf
                EndIf
                If TRB2->TMP_BAIXA == "S"
                    lBaixado := U_M0603I(TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cParcela,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALJUR,TRB2->TMP_CODMAR,cMsg,nOpcao)
                    If !lBaixado
                        AADD( aErros, { DTOC(TRB2->TMP_DTPGT),TRB2->TMP_CANAL,cMsg } )
                    EndIf
                EndIf
            EndIf
        If lLiquida .AND. !lBaixado
            AADD( aImprime, { TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,DTOC(TRB2->TMP_DTPGT),"Liquidação de título excluída." } )
        ElseIf !lLiquida .AND. lBaixado
            AADD( aImprime, { TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,DTOC(TRB2->TMP_DTPGT),"Baixa de título excluída." } )
        ElseIf lLiquida .AND. lBaixado
            AADD( aImprime, { TRB2->TMP_CLIENT,TRB2->TMP_LOJA,cPrefixo,TRB2->TMP_TITULO,cTpTit,TRB2->TMP_VALPAG,TRB2->TMP_VALCOM,DTOC(TRB2->TMP_DTPGT),"Baixa e liquidação de título excluída." } )
        EndIf
        lLiquida := .F.
        lBaixado := .F.
        TRB2->(dbSkip())
        End Transaction
    EndDo

    If Len(aImprime) > 0
        U_M0603T(aImprime)
    EndIf

    If Len(aErros)
        U_M0603U(aErros)
    EndIf

    RestArea(aArea)

Return

User Function M0603W(cPrefixo,cNumero,cParcela,cCliente,cLoja,dData)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local lRet   := .F.

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE5") + " E5 "
    cQuery += "WHERE E5.D_E_L_E_T_ = ' ' "
    cQuery += "AND E5_FILIAL = '02' "
    cQuery += "AND E5_DATA = '" + DTOS(dData) + "' "
    cQuery += "AND E5_PREFIXO = '" + Alltrim(cPrefixo) + "' "
    cQuery += "AND E5_NUMERO = '" + Alltrim(cNumero) + "' "
    cQuery += "AND E5_PARCELA = '" + Alltrim(cParcela) + "' "
    cQuery += "AND E5_CLIFOR = '" + Alltrim(cCliente) + "' "
    cQuery += "AND E5_LOJA = '" + Alltrim(cLoja) + "' "

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !((cAlias)->(Eof()))
        If (cAlias)->E5_MOTBX == "NOR"
            lRet := .T.
        EndIf
        (cAlias)->(dbSkip())
    EndDo

    (cAlias)->(dbCloseArea())

Return(lRet)
