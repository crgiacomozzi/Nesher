#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"
#INCLUDE "RestFul.ch"

//Variáveis Estáticas
Static cTitulo := "Integração PagSeguro X Koncili"
Static cAlias1 := "SC5"

/*/{Protheus.doc} A0602
    Função responsável pela inclusão de pedido do PagSeguro do Protheus na Koncili
    @type  Function
    @author Pono Tecnologia
    @since 08/09/2022
    @version 12.1.33
    /*/
User Function A0602()
    Local oBrowse
    Local nDiaLim   := SuperGetMV("MV_YDLIMKO",.F.,60)
    Local cDtLimit  := DTOS(DaySub(dDatabase,nDiaLim))
    Local cCondicao := "UPPER(Alltrim(SC5->C5_NOMMKT)) == 'PAGSEGURO' .AND. SC5->C5_EMISSAO >= '" + cDtLimit + "'"
    Private aRotina := MenuDef()

    oBrowse := FWMBrowse():New()
    oBrowse:SetAlias(cAlias1)
    oBrowse:SetDescription(cTitulo)
    oBrowse:AddLegend( "SC5->C5_YINTKO == '1'"                           , "GREEN",  "Pedido de vendas integrado"       , ,.T.)
    oBrowse:AddLegend( "SC5->C5_YINTKO == '2' .OR. SC5->C5_YINTKO == ' '", "RED"  ,  "Pedido de vendas não integrado"   , ,.T. )
    oBrowse:SetFilterDefault(cCondicao)
    oBrowse:Activate()

Return

/*/{Protheus.doc} MenuDef
    Função para montagem do menu
    @type Function
    @author Pono Tecnologia
    @since 08/09/2022
    @version 12.1.33
    /*/
Static Function MenuDef()
    Local aRotina := {}

    ADD OPTION aRotina TITLE "Pesquisar"  	            ACTION 'PesqBrw' 		  OPERATION 1 ACCESS 0
    ADD OPTION aRotina TITLE "Visualizar" 	            ACTION "VIEWDEF.PN_A0602" OPERATION 2 ACCESS 0
    ADD OPTION aRotina TITLE "Integrar Título Koncili"  ACTION "U_A0602A(SC5->C5_FILIAL,SC5->C5_NUM,SC5->C5_YINTKO,SC5->C5_YORDID)"       OPERATION 8 ACCESS 0
    ADD OPTION aRotina TITLE "Alterar Título Koncili"   ACTION "U_A0602E(SC5->C5_FILIAL,SC5->C5_NUM,SC5->C5_YINTKO,SC5->C5_YORDID)"       OPERATION 8 ACCESS 0
    ADD OPTION aRotina TITLE "Excluir Título Koncili"   ACTION "U_A0602F(SC5->C5_FILIAL,SC5->C5_NUM,SC5->C5_PN_IDFR,Alltrim(SC5->C5_NOMMKT),SC5->C5_YINTKO)"       OPERATION 8 ACCESS 0

Return aRotina

/*/{Protheus.doc} ModelDef
    Função para montar o modelo
    @type Function
    @author Pono Tecnologia
    @since 08/09/2022
    @version 12.1.33
    /*/
Static Function ModelDef()
    Local oModel := Nil
    Local oMaster  := FWFormStruct( 1, cAlias1 )

    oModel := MPFormModel():New('PN_A0602M',,,)
    oModel:addFields("MASTER", Nil, oMaster)
    oModel:SetPrimaryKey({"C5_FILIAL", "C5_NUM" })
    oModel:SetDescription(cTitulo)
    oModel:GetModel( "MASTER" ):SetDescription( "Formulário de cadastro" + cTitulo ) 
	
Return oModel

/*/{Protheus.doc} ViewDef
    Função para montar a visuzalização
    @type Function
    @author Cristiano Roberto Giacomozzi
    @since 11/10/2021
    @version 12.1.23
    /*/
Static Function ViewDef()
    Local oModel:= FWLoadModel( "PN_A0602" )
    Local oMaster	:= FWFormStruct( 2, cAlias1 )
    Local oView := Nil
    
    oView := FWFormView():New()
    oView:SetModel(oModel)
    oView:AddField( "VIEW_MASTER" , oMaster , 'MASTER' )
    oView:EnableTitleView('VIEW_MASTER', 'Integração PagSeguro X Koncili' )
    oView:SetCloseOnOk({||.T.})
    oView:EnableControlBar(.T.)
	
Return oView

/*/{Protheus.doc} A0602A
    Função responsável pelo envio das informações do pedido para API da Koncili (Inclusão)
    @type  Function
    @author Pono Tecnologia
    @since 09/09/2022
    @version 12.1.33
    @param cFilSC5 , Character, Filial do pedido de vendas
    @param cPEdSC5 , Character, Número do pedido de vendas
    @param cPedInt , Character, Campo que indica se o pedido já foi integrado com a Koncili
    @param cOrderId, Character, ID do pedido na Precode
    /*/
User Function A0602A(cFilSC5,cPedSC5,cPedInt,cOrderId)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest

    If cPedInt <> "1"
        cUrl  := SuperGetMV("MV_YKONURL",.F.,"")
        cPath := "/externalapi/order"
        cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

        Aadd(aHeader,'Accept: application/json')
        Aadd(aHeader,'Content-Type: application/json')
        AADD(aHeader, "gumgaToken: " + cAuth)

        oRest := FWRest():new(cUrl)
        oRest:setPath(cPath)
        oRest:SetPostParams(U_A0602B(cFilSC5,cPedSC5,cOrderId))

        If oRest:Post(aHeader)
            Aviso("Atenção!","Integração realizada com sucesso!!!.",{"Ok"})
            ConOut("POST", oRest:GetResult())
            dbSelectArea("SC5")
            SC5->(DbSetOrder(1))
            If SC5->(dbSeek(cFilSC5+cPedSC5))
                Reclock("SC5", .F.)
                    SC5->C5_YINTKO := '1'
                MsUnlock()
            EndIf
        Else
            Aviso("Atenção!","Erro ao realizar a integração " + oRest:GetLastError(),{"Ok"})
            ConOut("POST", oRest:GetLastError())
        EndIf

        FreeObj(oRest)
    Else
        Aviso("Atenção!","O título já foi integrado com a Koncili, deverá ser utilizada a rotina de alteração.",{"Ok"})
    EndIf

    FreeObj(oRest)

Return

/*/{Protheus.doc} A0602B
    Função responsável pela montagem do jSon que será enviado para a API da Koncili
    @type  Function
    @author Pono Tecnologia
    @since 09/09/2022
    @version 12.1.33
    @param cFilSC5, Character, Filial do pedido de vendas
    @param cPEdSC5, Character, Número do pedido de vendas
    @param cOrderId, Character, ID do pedido na Precode
    /*/
User Function A0602B(cFilSC5,cPedSC5,cOrderId)
    Local jJson     := JsonObject():New()
    Local aPed      := {}
    Local aItens    := {}
    Local aItjson   := {}
    Local aPgto     := {}
    Local aPgPre    := {}
    Local aCliente  := {}
    Local nTotal    := 0
    Local nX
    Local cDate
    Local cDtEnv
    Local cDtNf

    aPed := U_A0602C(cFilSC5,cPedSC5)
    aItens := U_A0602D(cFilSC5,cPedSC5)
    
    cDate    := SubStr(aPed[1,2],1,4) + "-" + SubStr(aPed[1,2],5,2) + "-" + SubStr(aPed[1,2],7,2)
    cDtEnv   := U_A0602G(aPed[1,3],aPed[1,4],aPed[1,6],aPed[1,7])
    cDtNf    := SubStr(cDtEnv,1,4) + "-" + SubStr(cDtEnv,5,2) + "-" + SubStr(cDtEnv,7,2)
    aCliente := U_A0602H(aPed[1,6],aPed[1,7])
    jJson["orderCode"]          := aPed[1,1]
    jJson["orderId"]                := aPed[1,1]
    jJson["orderDate"]              := cDate + "T00:00:00.420+0000"
    jJson["orderStatus"]            := "CONCLUDED"
    jJson["hubName"]                := "EXTERNAL_API"
    jJson["accountName"]            := "Nesher"
    jJson["channelName"]            := "PAGSEGURO"
    jJson["totalFreightValue"]      := aPed[1,5]
    jJson["invoiceNumber"]          := aPed[1,3]
    jJson["invoiceDate"]            := cDtNf + "T00:00:00.420+0000"
    jJson["customerName"]           := aCliente[1,1]
    jJson["customerDocumentNumber"] := aCliente[1,2]
    For nX := 1 to Len(aItens)
        AADD(aItjson,JsonObject():New())
        aItjson[nX]['idInMarketplace']  := aItens[nX,1]
        aItjson[nX]['code']             := aItens[nX,1]
        aItjson[nX]['quantity']         := aItens[nX,2]
        aItjson[nX]['unitValue']        := aItens[nX,3]
        aItjson[nX]['freightValue']     := 0
        aItjson[nX]['sendDate']         := cDate + "T00:00:00.420+0000" // Data da emissão da nota fiscal
        nTotal += aItens[nX,3]
    Next nX
    jJson["totalNetValue"] := nTotal + aPed[1,5]
        aPgPre := U_A0602I(cOrderId)
        AADD(aPgto,JsonObject():New())
        aPgto[1]['method']  := aPgPre[1,1]
        aPgto[1]['date']    := cDate + "T00:00:00.420+0000"
        aPgto[1]['value']   := nTotal + aPed[1,5]
        aPgto[1]['installmentQuantity']  := aPgPre[1,2]
    jJson["items"] := JsonObject():New()
    jJson["items"] := aItjson
    jJson["payments"] := JsonObject():New()
    jJson["payments"] := aPgto

return jJson:ToJson()

/*/{Protheus.doc} A0602C
    Função responsável por retornar detalhes do pedido de vendas
    @type  Function
    @author Pono Tecnologia
    @since 09/09/2022
    @version 12.1.33
    @param cFilSC5, Character, Filial do pedido de vendas
    @param cPEdSC5, Character, Número do pedido de vendas
    /*/
User Function A0602C(cFilSC5,cPedSC5)
    Local cAlias    := GetNextAlias()
    Local cQuery
    Local aPed      := {}

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SC5") + " C5 "
    cQuery += "WHERE C5.D_E_L_E_T_ = '' AND C5_FILIAL = '" + cFilSC5 + "' AND C5_NUM = '" + cPedSC5 + "' "
    
    MPSysOpenQuery( cQuery, cAlias )
	
    DbSelectArea(cAlias)
    (cAlias)->(DbGoTop())

    If !Empty((cAlias)->C5_NUM)
        AADD( aPed, {Alltrim((cAlias)->C5_PN_IDFR), (cAlias)->C5_EMISSAO, (cAlias)->C5_NOTA, (cAlias)->C5_SERIE, (cAlias)->C5_FRETE, (cAlias)->C5_CLIENTE, (cAlias)->C5_LOJACLI } )
    EndIf

    (cAlias)->(DbCloseArea())

Return(aPed)

/*/{Protheus.doc} A0602D
    Função responsável por retornar os itens do pedido de vendas
    @type  Function
    @author Pono Tecnologia
    @since 09/09/2022
    @version 12.1.33
    @param cFilSC5, Character, Filial do pedido de vendas
    @param cPEdSC5, Character, Número do pedido de vendas
    /*/
User Function A0602D(cFilSC5,cPedSC5)
    Local cAlias    := GetNextAlias()
    Local cQuery
    Local aItens    := {}

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SC6") + " C6 "
    cQuery += "WHERE C6.D_E_L_E_T_ = '' AND C6_FILIAL = '" + cFilSC5 + "' AND C6_NUM = '" + cPedSC5 + "' "
    
    MPSysOpenQuery( cQuery, cAlias )
	
    DbSelectArea(cAlias)
    (cAlias)->(DbGoTop())

    If !Empty((cAlias)->C6_NUM)
        AADD( aItens, {Alltrim((cAlias)->C6_PRODUTO), (cAlias)->C6_QTDVEN, (cAlias)->C6_PRCVEN } )
    EndIf

    (cAlias)->(DbCloseArea())

Return(aItens)

/*/{Protheus.doc} A0602E
    Função responsável pelo envio das informações do pedido para API da Koncili (Alteração)
    @type  Function
    @author Pono Tecnologia
    @since 12/09/2022
    @version 12.1.33
    @param cFilSC5 , Character, Filial do pedido de vendas
    @param cPEdSC5 , Character, Número do pedido de vendas
    @param cPedInt , Character, Campo que indica se o pedido já foi integrado com a Koncili
    @param cOrderId, Character, ID do pedido na Precode
    @param cPedInt, Character, Campo que indica se o título já está integrado com a Koncili
    /*/
User Function A0602E(cFilSC5,cPedSC5,cPedInt,cOrderId)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local cJson     := JsonObject():New()

    If cPedInt == "1"
        cUrl  := SuperGetMV("MV_YKONURL",.F.,"")
        cPath := "/externalapi/order"
        cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

        Aadd(aHeader,'Accept: application/json')
        Aadd(aHeader,'Content-Type: application/json')
        AADD(aHeader, "gumgaToken: " + cAuth)

        oRest := FWRest():new(cUrl)
        oRest:setPath(cPath)
        cJson := U_A0602B(cFilSC5,cPedSC5,cOrderId)

        If oRest:Put(aHeader,cJson)
            Aviso("Atenção!","Alteração realizada com sucesso!!!.",{"Ok"})
            ConOut("PUT", oRest:GetResult())
        Else
            Aviso("Atenção!","Erro ao realizar a integração " + oRest:GetLastError(),{"Ok"})
            ConOut("PUT", oRest:GetLastError())
        EndIf

        FreeObj(oRest)
    Else
        Aviso("Atenção!","O título ainda não foi integrado com a Koncili.",{"Ok"})
    EndIf

    FreeObj(oRest)

Return

/*/{Protheus.doc} A0602E
    Função responsável pela exclusão do título da API da Koncili
    @type  Function
    @author Pono Tecnologia
    @since 12/09/2022
    @version 12.1.33
    @param cFilSC5, Character, Filial do pedido de vendas
    @param cPEdSC5, Character, Número do pedido de vendas
    @param cOrder,  Character, Código do pedido no marketplace 
    @param cMarket, Character, Nome do marketplace
    @param cPedInt, Character, Campo que indica se o título já está integrado com a Koncili
    /*/
User Function A0602F(cFilSC5,cPedSC5,cOrder,cMarket,cPedInt)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest

    If cPedInt == "1"
        cUrl  := SuperGetMV("MV_YKONURL",.F.,"")
        cPath := "/externalapi/order/" + cOrder + "/" + cMarket
        cAuth := SuperGetMV("MV_YKONAUT",.F.,"205004666L1E1747261718188C165394971818800O1.I")

        Aadd(aHeader,'Accept: application/json')
        Aadd(aHeader,'Content-Type: application/json')
        AADD(aHeader, "gumgaToken: " + cAuth)

        oRest := FWRest():new(cUrl)
        oRest:setPath(cPath)

        If oRest:Delete(aHeader)
            Aviso("Atenção!","Alteração realizada com sucesso!!!.",{"Ok"})
            ConOut("DELETE", oRest:GetResult())
            dbSelectArea("SC5")
            SC5->(DbSetOrder(1))
            If SC5->(dbSeek(cFilSC5+cPedSC5))
                Reclock("SC5", .F.)
                    SC5->C5_YINTKO := '2'
                MsUnlock()
            EndIf
        Else
            Aviso("Atenção!","Erro ao realizar a integração " + oRest:GetLastError(),{"Ok"})
            ConOut("DELETE", oRest:GetLastError())
        EndIf

        FreeObj(oRest)
    Else
        Aviso("Atenção!","O título ainda não foi integrado com a Koncili.",{"Ok"})
    EndIf

    FreeObj(oRest)

Return

/*/{Protheus.doc} A0602G
    Função responsável por retornar a data da emissão da nota fiscal do pedido de vendas.
    @type  Function
    @author Pono Tecnologia
    @since 14/09/2022
    @version 12.1.33
    @param cNf      , Character, Núemro da nota fiscal
    @param cSerie   , Character, Série da nota fiscal
    @param cCliente , Character, Código do cliente
    @param cLoja    , Character, Loja do cliente
    @return Character, Data de emissão da nota fiscal do pedido de vendas
    /*/
User Function A0602G(cNf,cSerie,cCliente,cLoja)
    Local cAlias    := GetNextAlias()
    Local cQuery
    Local cData

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SF2") + " F2 "
    cQuery += "WHERE F2.D_E_L_E_T_ = '' AND F2_FILIAL = '02' AND F2_DOC = '" + cNf + "' AND F2_SERIE = '" + cSerie + "' AND F2_CLIENTE = '" + cCliente + "'AND F2_LOJA = '" + cLoja + "' "
    
    MPSysOpenQuery( cQuery, cAlias )
	
    DbSelectArea(cAlias)
    (cAlias)->(DbGoTop())

    If !Empty((cAlias)->F2_DOC)
        cData := (cAlias)->F2_EMISSAO
    EndIf

    (cAlias)->(DbCloseArea())

Return(cData)

/*/{Protheus.doc} A0602H
    Função responsável por retornar o nome do cliente do pedido de venda.
    @type  Function
    @author Pono Tecnologia
    @since 14/09/2022
    @version 12.1.33
    @param cCliente , Character, Código do cliente
    @param cLoja    , Character, Loja do cliente
    @return Character, Nome do cliente.
    /*/
User Function A0602H(cCliente,cLoja)
    Local cAlias    := GetNextAlias()
    Local cQuery
    Local aCliente  := {}

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SA1") + " A1 "
    cQuery += "WHERE A1.D_E_L_E_T_ = '' AND A1_FILIAL = '02' AND A1_COD = '" + cCliente + "'AND A1_LOJA = '" + cLoja + "' "
    
    MPSysOpenQuery( cQuery, cAlias )
	
    DbSelectArea(cAlias)
    (cAlias)->(DbGoTop())

    If !Empty(Alltrim((cAlias)->A1_NOME))
        AADD(aCliente, { Alltrim((cAlias)->A1_NOME), Alltrim((cAlias)->A1_CGC) } )
    EndIf

    (cAlias)->(DbCloseArea())

Return(aCliente)

/*/{Protheus.doc} A0602I
    Função responsável por obter as informações relacionadas ao pagamento do pedido de venda na Precode
    @type  Function
    @author Pono Tecnologia
    @since 15/09/2022
    @version 12.1.33
    @param cOrderId, Character, ID do pedido de vendas na Precode
    @return Array, Um array com informações de tipo e quantidade de parcelas do pagamento.
    /*/
User Function A0602I(cOrderId)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader := {}
    Local oRest
    Local aPgto   := {}
    Local cTpPgto
    Local cDesPgt
    Local nQtdPar

    cUrl  := "https://erp.precode.com.br"
    cPath := "/api/v1/erp/status/" + cOrderId
    
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
            cJson := StrTran(cJson,"ï»¿","")
            cParser  := oJson:FromJson(cJson)
		EndIf

        If Empty(cParser)
            cTpPgto := oJson['pedido'][1]['pagamento'][1]['sefaz']['idMeioPagamento']
            nQtdPar := oJson['pedido'][1]['pagamento'][1]['quantidadeParcelas']
            cDesPgt := U_A0602J(cTpPgto)
            AADD(aPgto, { cDesPgt, nQtdPar } )
        EndIf
    EndIf

    freeObj(oJson)
    freeObj(oRest)

Return(aPgto)

/*/{Protheus.doc} A0602J
    Função que retorna o tipo de pagamento conforme o tipo obtido pela API da Precode
    @type  Function
    @author Pono Tecnologia
    @since 15/09/2022
    @version 12.1.33
    @param cTpPgto, Character, ID da forma de pagamento
    @return Character, Descrição da foram de pagamento conforme API da Koncili
    /*/
User Function A0602J(cTpPgto)
    Local cDesc := ""

    If cTpPgto == "04"
        cDesc := "CREDIT_CARD"
    ElseIf cTpPgto == "15"
        cDesc := "BILLET"
    Else
        cDesc := "OTHER"
    EndIf
        
Return(cDesc)
