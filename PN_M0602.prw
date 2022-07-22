#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"
#INCLUDE "RestFul.ch"

/*/{Protheus.doc} nomeFunction
    Fun��o respons�vel pela baixa de t�tulos recebidos da API da Koncilli
    @type  Function
    @author Pono Tecnologia
    @since 25/05/2022
    @version 12.1.33
    /*/
User Function M0602()
    Local cCadastro     := "Processamento de concilia��es da Koncili"
    Private lRpc	    := Type("cFilAnt") == "U"
    Private aCpoInfo    := {}
    Private aCampos     := {}
    Private aCpoData    := {}
    Private oTable      := Nil
    Private oMarkBrow   := Nil
    Private aRotina     := MenuDef()

    If lRpc
		RPCSetType(3)
		RpcSetEnv('01','02',,,,GetEnvServer(),{ })
	Else 
        If cFilAnt == "01"
            Aviso("Aten��o!","Opera��o n�o permitida para a MATRIZ.",{"Ok"})
            Return
        EndIf
    EndIf

    SetKey (VK_F12,{|a,b| AcessaPerg("M0602",.T.)})

    pergunte("M0602",.F.)

    FwMsgRun(,{ || U_M0602A() }, cCadastro, 'Carregando dados...')

	oBrowse := FWmBrowse():New()
	oBrowse:SetAlias('TRB')
	oBrowse:SetDescription( cCadastro )
	//oBrowse:SetSeek(.T.,aSeek)
	oBrowse:SetTemporary(.T.)
	oBrowse:SetLocate()
	oBrowse:SetUseFilter(.T.)
	oBrowse:SetDBFFilter(.T.)
	oBrowse:SetFilterDefault( "" )

	//Legenda da grade, � obrigat�rio carregar antes de montar as colunas
	oBrowse:AddLegend("TMP_STATUS=='A'","GREEN" 	,"Concilia��o Pendente")
	oBrowse:AddLegend("TMP_STATUS=='B'","BLUE"  	,"Concilia��o Parcialmente Baixada")
	oBrowse:AddLegend("TMP_STATUS=='C'","RED"  	    ,"Concilia��o Baixada")

    oBrowse:SetColumns(aCampos)
	//oBrowse:SetFieldFilter(aFieFilter)
	oBrowse:DisableDetails()

    oBrowse:Activate()

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

Return

Static Function MenuDef
    Local aRot := {}

    ADD Option aRot Title 'Processar'               Action 'U_M0603(Alltrim(TRB->TMP_COD),Alltrim(TRB->TMP_CANAL))' Operation 0 Access 0
    ADD Option aRot Title 'Recarregar Concilia��es' Action 'FwMsgRun(,{ || U_M0602F() }, Recarregando concili��es de t�tulos da Koncili", "Carregando dados...")'   Operation 0 Access 0
    ADD Option aRot Title 'Atualizar Repasses'      Action 'U_M0604(Alltrim(TRB->TMP_COD),Alltrim(TRB->TMP_CANAL))' Operation 0 Access 0
    

Return(aRot)

User Function M0602A()
    Local nI, nX
    Local aCarrega := {}

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    oTable   := FwTemporaryTable():New('TRB')
    aCampos  := {}
    aCpoInfo := {}
    aCpoData := {}

    aAdd(aCpoInfo, {'Status'                , '@!' , 1})
    aAdd(aCpoInfo, {'C�digo'                , '@!' , 1})
    aAdd(aCpoInfo, {'Criado'                , '@D' , 1})
    aAdd(aCpoInfo, {'Per�odo'               , '@!' , 1})
    aAdd(aCpoInfo, {'Canal de venda'        , '@!' , 1})
    aAdd(aCpoInfo, {'Conta'                 , '@!' , 1})
    aAdd(aCpoInfo, {'Total resolvido'       , '@E 999,999,999.99' , 2})
    aAdd(aCpoInfo, {'Total a resolver'      , '@E 999,999,999.99' , 2})
    aAdd(aCpoInfo, {'Total'                 , '@E 999,999,999.99' , 2})

    aAdd(aCpoData, {'TMP_STATUS'    , 'C' , 01, 0})
    aAdd(aCpoData, {'TMP_COD'       , 'C' , 06, 0})
    aAdd(aCpoData, {'TMP_CRIA'      , 'D' , 08, 0})
    aAdd(aCpoData, {'TMP_PERIOD'    , 'C' , 20, 0})
    aAdd(aCpoData, {'TMP_CANAL'     , 'C' , 20, 0})
    aAdd(aCpoData, {'TMP_CONTA'     , 'C' , 20, 0})
    aAdd(aCpoData, {'TMP_TOTRE'     , 'N' , 14, 2})
    aAdd(aCpoData, {'TMP_TOTAR'     , 'N' , 14, 2})
    aAdd(aCpoData, {'TMP_TOTAL'     , 'N' , 14, 2})

    For nI := 1 To Len(aCpoData)

        If(aCpoData[nI][1] <> 'TMP_STATUS')

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

    aCarrega = U_M0602B()

    If Len(aCarrega) > 0

        For nX := 1 To Len(aCarrega)

            DbSelectArea('TRB')

            RecLock('TRB', .T.)

                TRB->TMP_STATUS   := aCarrega[nX,7]
                TRB->TMP_COD      := aCarrega[nX,1]
                TRB->TMP_CRIA     := STOD(aCarrega[nX,2])
                TRB->TMP_PERIOD   := aCarrega[nX,3]
                TRB->TMP_CANAL    := aCarrega[nX,4]
                TRB->TMP_CONTA    := aCarrega[nX,5]
                TRB->TMP_TOTRE    := aCarrega[nX,8]
                TRB->TMP_TOTAR    := aCarrega[nX,9]
                TRB->TMP_TOTAL    := aCarrega[nX,6]

            TRB->(MsUnlock())
        
        Next nX

        TRB->(DbGoTop())

    EndIf

Return

User Function M0602B()
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX, nY, nSoma
    Local cConcId
    Local cDtCria
    Local cConta
    Local cMarket
    Local nValRe    := 0
    Local nValAr    := 0
    Local nValor
    Local aBaixa    := {}
    Local dDtIni    := Substr(DTOS(MV_PAR01),1,4) + "-" + Substr(DTOS(MV_PAR01),5,2) + "-" + Substr(DTOS(MV_PAR01),7,2)
    Local dDtFim    := Substr(DTOS(MV_PAR02),1,4) + "-" + Substr(DTOS(MV_PAR02),5,2) + "-" + Substr(DTOS(MV_PAR02),7,2)
    Local nLimit    := 1
    Local nOffSet   := 1
    Local nCount    := 1
    Local nVezes    := 1
    Local cCod      := ""
    Local cCanal    := ""
    Local cTipo
    Local cStatus   := ""
    Local lBaixado  := .F.
    Local lLiquida  := .F.

    If !Empty(Alltrim(DTOS(MV_PAR01))) .AND. !Empty(Alltrim(DTOS(MV_PAR02)))
        U_M0602C(@nLimit,@nOffSet,@nCount,@nVezes,@cCod,@cCanal)

        For nY := 1 to nVezes
            cUrl  := "https://api-sandbox.koncili.com"
            cPath := "/externalapi/orderextract/unresolveds?offset="+ Alltrim(Str(nOffSet)) + "&initDate=" + dDtIni + "&endDate=" + dDtFim
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
                    cJson := StrTran(cJson,"﻿","")
                    cParser  := oJson:FromJson(cJson)
                EndIf
                If Empty(cParser)
                    For nX := 1 To Len(oJson['content'])
                        cOrder  := oJson['content'][nX]['orderCode']
                        cConcId := Alltrim(Str(oJson['content'][nX]['conciliationId']))
                        cDtCria := StrTran(SubStr(oJson['content'][nX]['conciliationCloseDate'],1,10),"-","")
                        cDtIni  := StrTran(SubStr(oJson['content'][nX]['conciliationInitDate'],1,10),"-","")
                        cDtFim  := StrTran(SubStr(oJson['content'][nX]['conciliationEndDate'],1,10),"-","")
                        cMarket := oJson['content'][nX]['channel']
                        cConta  := Strtran(NoAcento(DecodeUTF8(oJson['content'][nX]['accountName'])),"�?","A")
                        cTipo   := oJson['content'][nX]['extractType']
                        nValAr  := oJson['content'][nX]['releasedValue']
                        cData   := StrTran(SubStr(oJson['content'][nX]['releasedDate'],1,10),"-","")
                        aTitulo := U_M0602D(cOrder,@lBaixado,@lLiquida)
                        cStatus := U_M0602E(lBaixado,lLiquida)
                        nPosField := 0
                        nPosField := aScan( aBaixa, {|x| AllTrim(x[1]) == Alltrim(cConcId) } )
                        If AllTrim(cTipo) != "TRANSFER"
                            If nPosField == 0
                                AADD( aBaixa, { cConcId,cDtCria,DTOC(STOD(cDtIni)) + "-" + DTOC(STOD(cDtFim)),cMarket,cConta,nValor,cStatus,nValRe,nValAr } )
                            Else 
                                aBaixa[nPosField,9] += nValAr
                                If (aBaixa[nPosField,7] != cStatus)
                                    aBaixa[nPosField,7] := "B"
                                EndIf
                            EndIf
                        EndIf
                    Next nX
                EndIf
            EndIf
            nOffSet += nLimit
            freeObj(oJson)
        Next nY
    EndIf

    aBaixa := U_M0602H(aBaixa)

    For nSoma := 1 To Len(aBaixa)
        aBaixa[nSoma,6] := aBaixa[nSoma,8] + aBaixa[nSoma,9]
    Next nSoma

Return(aBaixa)

User Function M0602C(nLimit,nOffSet,nCount,nVezes,cCod,cCanal)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local dDtIni    := Substr(DTOS(MV_PAR01),1,4) + "-" + Substr(DTOS(MV_PAR01),5,2) + "-" + Substr(DTOS(MV_PAR01),7,2)
    Local dDtFim    := Substr(DTOS(MV_PAR02),1,4) + "-" + Substr(DTOS(MV_PAR02),5,2) + "-" + Substr(DTOS(MV_PAR02),7,2)
    
    If !Empty(Alltrim(DTOS(MV_PAR01))) .AND. !Empty(Alltrim(DTOS(MV_PAR02)))
        cUrl  := "https://api-sandbox.koncili.com"
        cPath := "/externalapi/orderextract/unresolveds?initDate=" + dDtIni + "&endDate=" + dDtFim
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
                cJson := StrTran(cJson,"﻿","")
                cParser  := oJson:FromJson(cJson)
            EndIf

            If Empty(cParser)
                nLimit  := oJson['page']['limit']
                nOffSet := oJson['page']['offset']
                nCount  := oJson['page']['count']
            EndIf

        EndIf

        nVezes := Ceiling(nCount/nLimit)
    EndIf

Return

User Function M0602D(cWareId,lBaixado,lLiquida)
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
        AADD( aRet, { (cAlias)->E1_CLIENTE, (cAlias)->E1_LOJA, (cAlias)->E1_NOMCLI, (cAlias)->E1_NUM } )
        lBaixado := U_M0603K(cWareId,MV_PAR03)
        lLiquida := U_M0603L((cAlias)->E1_CLIENTE,(cAlias)->E1_LOJA,(cAlias)->E1_NUM)
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(aRet)

User Function M0602E(lBaixado,lLiquida)
    Local cStatus := ""

    If lBaixado .AND. lLiquida
        cStatus := "C"
    ElseIf lBaixado .AND. !lLiquida
        cStatus := "B"
    ElseIf !lBaixado .AND. lLiquida
        cStatus := "B"
    ElseIf !lBaixado .AND. !lLiquida
        cStatus := "A"
    EndIf

Return(cStatus)

User Function M0602F()

    FwMsgRun(,{ || U_M0602A() }, "Processamento de concilia��es da Koncilli", "Carregando dados...")

Return

User Function M0602G()

    msginfo("Processando atualiza��o de repasse Koncili....")

Return

User Function M0602H(aBaixa)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX, nY
    Local cConcId
    Local cDtCria
    Local cConta
    Local cMarket
    Local nValRe    := 0
    Local nValAr    := 0
    Local nValor
    Local dDtIni    := Substr(DTOS(MV_PAR01),1,4) + "-" + Substr(DTOS(MV_PAR01),5,2) + "-" + Substr(DTOS(MV_PAR01),7,2)
    Local dDtFim    := Substr(DTOS(MV_PAR02),1,4) + "-" + Substr(DTOS(MV_PAR02),5,2) + "-" + Substr(DTOS(MV_PAR02),7,2)
    Local nLimit    := 1
    Local nOffSet   := 1
    Local nCount    := 1
    Local nVezes    := 1
    Local cCod      := ""
    Local cCanal    := ""
    Local cTipo
    Local cStatus   := ""
    Local lBaixado  := .F.
    Local lLiquida  := .F.

    If !Empty(Alltrim(DTOS(MV_PAR01))) .AND. !Empty(Alltrim(DTOS(MV_PAR02)))
        U_M0602C(@nLimit,@nOffSet,@nCount,@nVezes,@cCod,@cCanal)

        For nY := 1 to nVezes
            cUrl  := "https://api-sandbox.koncili.com"
            cPath := "/externalapi/orderextract/concilieds?offset="+ Alltrim(Str(nOffSet)) + "&initDate=" + dDtIni + "&endDate=" + dDtFim
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
                    cJson := StrTran(cJson,"﻿","")
                    cParser  := oJson:FromJson(cJson)
                EndIf
                If Empty(cParser)
                    For nX := 1 To Len(oJson['content'])
                        cOrder  := oJson['content'][nX]['orderCode']
                        cConcId := Alltrim(Str(oJson['content'][nX]['conciliationId']))
                        cDtCria := StrTran(SubStr(oJson['content'][nX]['conciliationCloseDate'],1,10),"-","")
                        cDtIni  := StrTran(SubStr(oJson['content'][nX]['conciliationInitDate'],1,10),"-","")
                        cDtFim  := StrTran(SubStr(oJson['content'][nX]['conciliationEndDate'],1,10),"-","")
                        cMarket := oJson['content'][nX]['channel']
                        cConta  := Strtran(NoAcento(DecodeUTF8(oJson['content'][nX]['accountName'])),"�?","A")
                        cTipo   := oJson['content'][nX]['extractType']
                        nValRe  := oJson['content'][nX]['releasedValue']
                        cData   := StrTran(SubStr(oJson['content'][nX]['releasedDate'],1,10),"-","")
                        aTitulo := U_M0602D(cOrder,@lBaixado,@lLiquida)
                        cStatus := U_M0602E(lBaixado,lLiquida)
                        nPosField := 0
                        nPosField := aScan( aBaixa, {|x| AllTrim(x[1]) == Alltrim(cConcId) } )
                        If AllTrim(cTipo) != "TRANSFER"
                            If nPosField == 0
                                AADD( aBaixa, { cConcId,cDtCria,DTOC(STOD(cDtIni)) + "-" + DTOC(STOD(cDtFim)),cMarket,cConta,nValor,cStatus,nValRe,nValAr } )
                            Else 
                                aBaixa[nPosField,8] += nValRe
                                If (aBaixa[nPosField,7] != cStatus)
                                    aBaixa[nPosField,7] := "B"
                                EndIf
                            EndIf
                        EndIf
                    Next nX
                EndIf
            EndIf
            nOffSet += nLimit
            freeObj(oJson)
        Next nY
    EndIf

Return(aBaixa)
