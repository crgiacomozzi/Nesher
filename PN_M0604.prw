#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"
#INCLUDE "RestFul.ch"

/*/{Protheus.doc} M0604
    Função responsável pelo envio da resolução do título no ERP
    @type  Function
    @author Pono Tecnologia
    @since 14/07/2022
    @version 12.1.33
    /*/
User Function M0604(cCod,cCanal)
    Local aArea         := GetArea()
    Private aCpoInfo    := {}
    Private aCampos     := {}
    Private aCpoData    := {}
    Private oTable      := Nil
    Private oMarkBrw4   := Nil
    Private aRotina     := MenuDef()
    Private dDtIni
    Private dDtFim
    Private dDtBaixa
    Private cConcId
    Private aCarrega    := {}
    Private aMovBan     := {}

    Pergunte("M0602",.T.)

    dDtIni    := MV_PAR01
    dDtFim    := MV_PAR02
    dDtBaixa  := MV_PAR03
    cConcId   := Alltrim(MV_PAR04)

    If !Empty(Alltrim(cConcId))
        nOpc := Aviso( "Resolução de títulos", "Confirma resolução dos títulos da conciliação ID " + Alltrim(cConcId), { "Sim", "Não"} , 2)
        If nOpc == 2
            Return
        EndIf
    Else
        Aviso( "Resolução de títulos", "O parâmetro ID da conciliação é obrigatório", { "Sim"} , 1)
        Return
    EndIf

    FwMsgRun(,{ || U_M0604B(cCod,cCanal,@aCarrega) }, "Processamento de títulos da Koncili", 'Carregando dados...')

    If Len(aCarrega) > 0
        FwMsgRun(,{ || U_M0604C(cCanal,cCod) }, "Enviando resolução dos títulos selecionados para Koncili", "Aguarda...")
    EndIf

    FwMsgRun(,{ || U_M0604G(cCanal,dDtIni,dDtFim,cConcId) }, "Finalizando resolução dos títulos para Koncili", "Aguarda...")

    RestArea(aArea)

    Aviso( "Resolução de títulos", "A conciliação foi finalizada " + cConcId + " na Koncili", { "Sim"} , 1)

Return

User Function M0604B(cCod,cCanal,aCarrega)
    Local nX, nZ
    Local aTitulos    := {}
    Local nValPag   := 0.00
    Local nValLiq   := 0.00
    Local nValJur   := 0.00
    Local nValDes   := 0.00

    aMovBan := {}

    If !Empty(Alltrim(DTOS(dDtIni))) .AND. !Empty(Alltrim(DTOS(dDtFim)))
        For nZ := 1 To Len(aNaoRes)
            if Alltrim(aNaoRes[nZ,14]) == Alltrim(cCanal) .AND. Alltrim(aNaoRes[nZ,15]) == Alltrim(cCod) .AND. aNaoRes[nZ,16]
                AADD(aTitulos, {aNaoRes[nZ,1],aNaoRes[nZ,2],aNaoRes[nZ,3],aNaoRes[nZ,4],aNaoRes[nZ,5],aNaoRes[nZ,6],aNaoRes[nZ,7],aNaoRes[nZ,8],aNaoRes[nZ,9],aNaoRes[nZ,10],aNaoRes[nZ,11],aNaoRes[nZ,12],aNaoRes[nZ,13],aNaoRes[nZ,14],aNaoRes[nZ,17],aNaoRes[nZ,18]})
                nX := Len(aTitulos)
                nValPag := 0.00
                nValLiq := 0.00
                nValJur := 0.00
                nValDes := 0.00
                U_M0603D(aTitulos[nX,10],cCanal,@nValPag,@nValLiq,@nValJur,@nValDes,aTitulos[nX,15])
                aTitulos[nX,6] += nValPag
                aTitulos[nX,7] += nValLiq
                aTitulos[nX,8] += nValJur
                aTitulos[nX,16] += nValDes
            EndIf
        Next nZ
    
    EndIf

    aCarrega := aClone(aTitulos)

Return

User Function M0604C(cCanal,cCod)
    Local aItens := {}
    Local aAux   := {}
    Local nX, nY, nZ, nW, nA
    Local cIds   := ""
    Local nTotal := 0
    Local nLimit := 100
    Local nLimTot:= nLimit
    Local nRecno := 0
    Local nPosMkt 

    For nA := 1 To Len(aCarrega)
        If aCarrega[nA,6] != 0
            U_M0604D(cCanal,aCarrega[nA,10],@aItens)
        EndIf
    Next nA

    nTotal := Len(aItens)

    For nX := 1 To nTotal
        cIds += Alltrim(Str(aItens[nX,1])) + Iif(nX != nTotal .AnD. nX != nLimTot,",","")
        If nX == nTotal .OR. nX == nLimTot
            cIds := "[" + cIds + "]"
            U_M0604E(cIds)
            cIds := ""
            nLimTot += nLimit
        EndIf
    Next nX

    If Len(aItens) > 0
        For nW := 1 to Len(aItens)
            nPosMkt := 0
            nPosMkt := aScan( aAux, {|x| AllTrim(x[1]) == Alltrim(aItens[nW,2]) } )
            If nPosMkt == 0
                AADD(aAux, {aItens[nW,2]} )
            EndIf
        Next nW
    EndIf    
    
    If Len(aAux) > 0
        For nZ := 1 to Len(aAux)
            nRecno := 0
            nRecno := U_M0604F(Alltrim(aAux[nZ,1]))
            If nRecno > 0
                DbSelectArea("SE1")
                SE1->(dbGoTo(nRecno))
                RecLock("SE1",.F.)
                    SE1->E1_RESOLKO := "S"
                SE1->(MsUnlock())
                nPosMkt := 0
                nPosMkt := aScan( aCarrega, {|x| AllTrim(x[10]) == Alltrim(aAux[nZ,1]) } )
                If nPosMkt > 0
                    dbSelectArea("ZZC")
                    ZZC->(DbSetOrder(1))
                    RecLock("ZZC",.T.)
                        ZZC->ZZC_FILIAL := "02"
                        ZZC->ZZC_MARKET := cCanal
                        ZZC->ZZC_IDCONC := cConcId
                        ZZC->ZZC_CLIENT := aCarrega[nPosMkt,1]
                        ZZC->ZZC_LOJA   := aCarrega[nPosMkt,2]
                        ZZC->ZZC_NOMCLI := aCarrega[nPosMkt,3]
                        ZZC->ZZC_TITULO := aCarrega[nPosMkt,4]
                        ZZC->ZZC_VALOR  := aCarrega[nPosMkt,5]
                        ZZC->ZZC_VALPAG := aCarrega[nPosMkt,6]
                        ZZC->ZZC_VALCOM := aCarrega[nPosMkt,7]
                        ZZC->ZZC_JUROS  := aCarrega[nPosMkt,8]
                        ZZC->ZZC_DTPAGT := STOD(aCarrega[nPosMkt,9])
                        ZZC->ZZC_CODMKT := aCarrega[nPosMkt,10]
                    ZZC->(MsUnlock())
                EndIf
            EndIf
        Next nZ
    EndIf

    If Len(aSemTit) > 0
        nTotal := Len(aSemTit)
        For nY := 1 To nTotal
            cIds += Alltrim(Str(aSemTit[nY,5])) + Iif(nY != nTotal .AnD. nY != nLimit,",","")
            If nY == nTotal .OR. nY == nLimit
                cIds := "[" + cIds + "]"
                U_M0604E(cIds)
                cIds := ""
            EndIf
        Next nY
    EndIf

Return

User Function M0604D(cCanal,cOrder,aItens)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX
    
    cUrl  := SuperGetMV("MV_YKONURL",.F.,"")
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
                cId     := oJson['elements'][nX]['id']
                cIdConc := oJson['elements'][nX]['conciliationId']
                If ValType(cIdConc) == "N"
                    cIdConc := Alltrim(Str(cIdConc))
                Else
                    cIdConc := ""
                EndIf
                If !Empty(cIdConc)
                    If cConcId == cIdConc
                        AADD( aItens, { cId, cOrder } )
                    EndIf
                EndIf
            Next nX
        EndIf
    EndIF

    freeObj(oJson)

Return

User Function M0604E(cIds)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest

    cUrl  := SuperGetMV("MV_YKONURL",.F.,"")
    cPath := "/externalapi/orderextract/resolve/batch"
    cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

    Aadd(aHeader,'Accept: application/json')
    Aadd(aHeader,'Content-Type: application/json')
    AADD(aHeader, "gumgaToken: " + cAuth)

    oRest := FWRest():new(cUrl)
    oRest:setPath(cPath)

    If (oRest:Put(aHeader, cIds))
        ConOut("PUT: " + oRest:GetResult())
    Else
        ConOut("PUT: " + oRest:GetLastError())
    EndIf

    FreeObj(oRest)

Return

User Function M0604F(cOrder)
    Local cAlias   := GetNextAlias()
    Local cQuery
    Local nRecno   := 0

    cQuery := "SELECT R_E_C_N_O_ AS RECNO "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '" + xFilial("SE1") + "' "
    cQuery += "AND E1_NUMLIQ != '' "
    cQuery += "AND E1_IDWARE = '" + cOrder + "' "

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If (cAlias)->RECNO > 0
        nRecno := (cAlias)->RECNO
    EndIf
    
    (cAlias)->(dbCloseArea())    

Return(nRecno)

User Function M0604G(cCanal,dDtIni,dDtFim,cConcId)
    Local aResolve  := {}
    Local cIds      := ""
    Local nX

    U_M0604H(cCanal,dDtIni,dDtFim,cConcId,@aResolve)

    If Len(aResolve) > 0
        nTotal := Len(aResolve)
        For nX := 1 To nTotal
            cIds += Alltrim(Str(aResolve[nX,1])) + Iif(nX != nTotal,",","")
            If nX == nTotal
                cIds := "[" + cIds + "]"
                U_M0604E(cIds)
            EndIf
        Next nX
    EndIf

Return

/*/{Protheus.doc} M0604H
    Função responsável por buscar pedidos ainda não resolvidos na Koncili
    @type  Function
    @author Pono Tecnologia
    @since 30/09/2022
    @version 12.1.33+
    /*/
User Function M0604H(cCanal,dDtIni,dDtFim,cConcId,aResolve)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX
    Local nId       := 0
    Local lTitulo   := .F.
    Local nOffSet   := 1
    Local cDtIni    := Substr(DTOS(dDtIni),1,4) + "-" + Substr(DTOS(dDtIni),5,2) + "-" + Substr(DTOS(dDtIni),7,2)
    Local cDtFim    := Substr(DTOS(dDtFim),1,4) + "-" + Substr(DTOS(dDtFim),5,2) + "-" + Substr(DTOS(dDtFim),7,2)


    cUrl  := SuperGetMV("MV_YKONURL",.F.,"")
    cPath := "/externalapi/orderextract/unresolveds?offset="+ Alltrim(Str(nOffSet)) + "&initDate=" + cDtIni + "&endDate=" + cDtFim + "&conciliationId=" + cConcId
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
                lTitulo := .F.
                cOrder  := oJson['content'][nX]['orderCode']
                nId     := oJson['content'][nX]['id']
                lTitulo := U_M0604I(cOrder)
                If !lTitulo
                    AADD( aResolve, { nId } )
                EndIf
            Next nX
        EndIf
    EndIf

    freeObj(oJson)

Return

User Function M0604I(cWareId)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local lRet   := .F.

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '02'
    cQuery += "AND E1_IDWARE = '" + Alltrim(cWareId) + "'

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty((cAlias)->E1_IDWARE)
        lRet := .T.
    EndIf
    
    (cAlias)->(dbCloseArea())

Return(lRet)
