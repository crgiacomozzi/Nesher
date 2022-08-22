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
    Private oMarkBrow   := Nil
    Private aRotina     := MenuDef()
    Private aMovBan     := {}
    Private dDtIni
    Private dDtFim

    Pergunte("M0602",.T.)

    dDtIni    := MV_PAR01
    dDtFim    := MV_PAR02

    FwMsgRun(,{ || U_M0604A(cCod,cCanal) }, "Processamento de títulos da Koncili", 'Carregando dados...')

    oMarkBrow := FwMarkBrowse():New()
    oMarkBrow:SetAlias('TRB3')
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
    oMarkBrow:SetAllMark( { || U_M0604J(oMarkBrow:Mark(),lMarcar := !lMarcar ), oMarkBrow:Refresh(.T.)  } )
    oMarkBrow:Activate()
    
    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    RestArea(aArea)

Return

User Function M0604A(cCod,cCanal)
    Local nI, nX
    Local aCarrega := {}

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    oTable   := FwTemporaryTable():New('TRB3')
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

    aCarrega = U_M0603B(cCod,cCanal,dDtIni,dDtFim)

    If Len(aCarrega) > 0

        For nX := 1 To Len(aCarrega)

            DbSelectArea('TRB3')

            RecLock('TRB3', .T.)

                TRB3->TMP_CANAL    := cCanal
                TRB3->TMP_IDCONC   := cCod
                TRB3->TMP_CLIENT   := aCarrega[nX,1]
                TRB3->TMP_LOJA     := aCarrega[nX,2]
                TRB3->TMP_NOME     := aCarrega[nX,3]
                TRB3->TMP_TITULO   := aCarrega[nX,4]
                TRB3->TMP_VALOR    := aCarrega[nX,5]
                TRB3->TMP_VALPAG   := aCarrega[nX,6]
                TRB3->TMP_VALCOM   := aCarrega[nX,7]
                TRB3->TMP_VALJUR   := aCarrega[nX,8]
                TRB3->TMP_DTPGT    := STOD(aCarrega[nX,9])
                TRB3->TMP_CODMAR   := aCarrega[nX,10]
                TRB3->TMP_STATUS   := aCarrega[nX,11]
                TRB3->TMP_BAIXA    := aCarrega[nX,12]
                TRB3->TMP_LIQUID   := aCarrega[nX,13]

            TRB3->(MsUnlock())
        
        Next nX

        TRB3->(DbGoTop())

    EndIf

Return

Static Function MenuDef()
    Local aRot := {}

    ADD Option aRot Title 'Resolver'  Action 'FwMsgRun(,{ || U_M0604B(), CloseBrowse() }, "Enviando resolução dos títulos selecionados para Koncili", "Aguarda...")' Operation 1 Access 0
    ADD Option aRot Title 'Visualizar' Action 'FwMsgRun(,{ || U_M0603N(TRB3->TMP_CANAL,TRB3->TMP_CODMAR) }, "Processando dos títulos selecionados", "Aguarda...")' Operation 2 Access 0

Return(aRot)

User Function M0604B()
    Local cCod
    Local cCanal
    Local aItens := {}
    Local nX, nY, nZ
    Local cIds   := ""
    Local nTotal := 0
    Local nLimit := 100
    Local nVezes := 0
    Local cRecno := ""

    TRB3->(dbGoTop())

    cCod    := Alltrim(TRB3->TMP_IDCONC)
    cCanal  := Alltrim(TRB3->TMP_CANAL)

    While !TRB3->(Eof())
        If !Empty(TRB3->TMP_OK)
            U_M0604C(cCanal,Alltrim(TRB3->TMP_CODMAR),@aItens)
        EndIf
        TRB3->(dbSkip())
    EndDo

    nTotal := Len(aItens)
    nVezes := Ceiling(nTotal/nLimit)

    For nY := 1 to nVezes
        For nX := 1 To nTotal
            cIds += Alltrim(Str(aItens[nX,1])) + Iif(nTotal != nX .AND. nX != nLimit,",","")
            If nTotal == nX .OR. nX == nLimit
                cIds := "[" + cIds + "]"
                U_M0604D(cIds)
                cIds := ""
            EndIf
        Next nX
    Next nY

    If Len(aItens) > 0
        For nZ := 1 to Len(aItens)
            cRecno := ""
            cRecno := U_M0604E(Alltrim(aItens[nZ,2]))
            If !Empty(Alltrim(cRecno))
                DbSelectArea("SE1")
                SE1->(dbGoTo(cRecno))
                RecLock("SE1",.F.)
                    SE1->E1_RESOLKO := "S"
                SE1->(MsUnlock())
            EndIf
        Next nZ
    EndIf

Return

User Function M0604C(cCanal,cOrder,aItens)
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
                cId    := oJson['elements'][nX]['id']
                AADD( aItens, { cId, cOrder } )
            Next nX
        EndIf
    EndIF

    freeObj(oJson)

Return

User Function M0604D(cIds)
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

Return

User Function M0604E(cOrder)
    Local cAlias   := GetNextAlias()
    Local cQuery
    Local cRecno   := ""

    cQuery := "SELECT R_E_C_N_O_ AS RECNO "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '" + xFilial("SE1") + "' "
    cQuery += "AND E1_NUMLIQ != '' "
    cQuery += "AND E1_IDWARE = '" + cOrder + "' "

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty(Alltrim((cAlias)->RECNO))
        cRecno := (cAlias)->RECNO
    EndIf
    
    (cAlias)->(dbCloseArea())    

Return(aCarrega)
