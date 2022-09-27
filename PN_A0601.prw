#INCLUDE "Totvs.ch"
#INCLUDE "FWMVCDef.ch"

//Variáveis Estáticas
Static cTitulo := "Cadastro de Marketplaces"
Static cAlias1 := "ZZA"
Static cAlias2 := "ZZB"
Static cAlias3 := "ZZE"

/*/{Protheus.doc} A0601
    Função responsável manutenção do cadastro de marketplaces e suas ocorrênicas.
    @type  Function
    @author Pono Tecnologia
    @since 28/06/2022
    @version 12.1.33
    /*/
User Function A0601()
    Local oBrowse
    Private aRotina := MenuDef()

    oBrowse := FWMBrowse():New()
    oBrowse:SetAlias(cAlias1)
    oBrowse:SetDescription(cTitulo)
    oBrowse:Activate()

Return Nil

/*/{Protheus.doc} MenuDef
    Função para montagem do menu
    @type Function
    @author Pono Tecnologia
    @since 28/06/2022
    @version 12.1.33
    /*/
Static Function MenuDef()
    Local aRotina := {}

    ADD OPTION aRotina TITLE "Pesquisar"  	            ACTION 'PesqBrw' 		  OPERATION 1 ACCESS 0
    ADD OPTION aRotina TITLE "Visualizar" 	            ACTION "VIEWDEF.PN_A0601" OPERATION 2 ACCESS 0
    ADD OPTION aRotina TITLE "Incluir"    	            ACTION "VIEWDEF.PN_A0601" OPERATION 3 ACCESS 0
    ADD OPTION aRotina TITLE "Alterar"    	            ACTION "VIEWDEF.PN_A0601" OPERATION 4 ACCESS 0
    ADD OPTION aRotina TITLE "Excluir"    	            ACTION "VIEWDEF.PN_A0601" OPERATION 5 ACCESS 0
    ADD OPTION aRotina TITLE "Imprimir" 	            ACTION "VIEWDEF.PN_A0601" OPERATION 8 ACCESS 0
    ADD OPTION aRotina TITLE "Busca Marketplace"        ACTION "U_A0601A()"       OPERATION 8 ACCESS 0

Return aRotina

/*/{Protheus.doc} ModelDef
    Função para montar o modelo
    @type Function
    @author Pono Tecnologia
    @since 28/06/2022
    @version 12.1.33
    /*/
Static Function ModelDef()
    Local oModel    := Nil
    Local oMaster   := FWFormStruct( 1, cAlias1 )
    Local oFilho1   := FWFormStruct( 1, cAlias2 )
    Local oFilho2   := FWFormStruct( 1, cAlias3 )

    //Criando o modelo
    oModel := MPFormModel():New('PN_A0601M',,,)

    oModel:addFields("MASTER", Nil, oMaster)

    // Criando a grid do FILHO1 e FILHO2
    oModel:AddGrid('FILHO1', 'MASTER', oFilho1 ,,,,)
    oModel:AddGrid('FILHO2', 'MASTER', oFilho2 ,,,,)

    // Criando o relacionamento do FILHO 1
    oModel:SetRelation( 'FILHO1' ,{{'ZZB_FILIAL','xFilial("ZZB")'},{'ZZB_MKTID','ZZA_MKTID'}},ZZB->(IndexKey(1)))
    oModel:GetModel('FILHO1'):SetUniqueLine({"ZZB_FILIAL","ZZB_MKTID","ZZB_ITEM"})

    // Criando o relacionamento do FILHO 2
    oModel:SetRelation( 'FILHO2' ,{{'ZZE_FILIAL','xFilial("ZZE")'},{'ZZE_MKTID','ZZA_MKTID'}},ZZE->(IndexKey(1)))
    oModel:GetModel('FILHO2'):SetUniqueLine({"ZZE_FILIAL","ZZE_CODFOR","ZZE_LOJFOR"})

   	// Permite grid vazio
    oModel:GetModel('FILHO1'):SetOptional( .T. )
    oModel:GetModel('FILHO2'):SetOptional( .T. )

    oModel:SetPrimaryKey({"ZZA_FILIAL", "ZZA_MKTID" })
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
    Local oModel:= FWLoadModel( "PN_A0601" )
    Local oMaster	:= FWFormStruct( 2, cAlias1 )
    Local oFilho1	:= FWFormStruct( 2, cAlias2 )
    Local oFilho2	:= FWFormStruct( 2, cAlias3 )
    Local oView := Nil
    
    oView := FWFormView():New()
    oView:SetModel(oModel)
    
    oFilho1:RemoveField('ZZB_MKTID')
    oFilho2:RemoveField('ZZE_MKTID')

    oView:AddUserButton( 'Busca ocorrências', 'CLIPS', { | oView| U_A0601B(FWFldGet("ZZA_MKTID")) } ,,,{ MODEL_OPERATION_INSERT,MODEL_OPERATION_UPDATE } )

    oView:AddField( "VIEW_MASTER" , oMaster , 'MASTER' )
    oView:EnableTitleView('VIEW_MASTER', 'Cadastro das ocorrência dos marketplaces' )

    oView:AddGrid( 'VIEW_FILHO1', oFilho1 , 'FILHO1' )
    oView:AddGrid( 'VIEW_FILHO2', oFilho2 , 'FILHO2' )

    oView:CreateHorizontalBox('SUPERIOR', 50)
    oView:CreateHorizontalBox('INFERIOR', 50)
    
    oView:SetCloseOnOk({||.T.})
    
    oView:CreateFolder('PASTA_FILHOS', 'INFERIOR')
    oView:AddSheet('PASTA_FILHOS', 'ABA_FILHO01', "Cadastro de ocorrências")
    oView:AddSheet('PASTA_FILHOS', 'ABA_FILHO02', "Cadastro de fornecedores")
    
    oView:CreateHorizontalBox('ITENS_FILHO01', 100,,, 'PASTA_FILHOS', 'ABA_FILHO01' )
    oView:CreateHorizontalBox('ITENS_FILHO02', 100,,, 'PASTA_FILHOS', 'ABA_FILHO02' )

    oView:SetOwnerView('VIEW_MASTER', 'SUPERIOR')
    oView:SetOwnerView('VIEW_FILHO1', 'ITENS_FILHO01')
    oView:SetOwnerView('VIEW_FILHO2', 'ITENS_FILHO02')
    
    oView:AddIncrementField('VIEW_FILHO1', 'ZZB_ITEM')
    oView:AddIncrementField('VIEW_FILHO2', 'ZZE_ITEM')

    oView:EnableControlBar(.T.)    
	
Return oView

User Function A0601A()
    Local aFields   := { " ", " " ,"Marktplace" }
    Local aButtons  := {}
    Local aItens    := {}
    Local oOk       := LoadBitMap(GetResources(), "LBOK")
    Local oNo       := LoadBitMap(GetResources(), "LBNO")

    U_A0601C(@aItens)

    If Len(aItens) > 0
        oDlg := FWDialogModal():New()

        oDlg:SetEscClose(.T.)
        oDlg:SetTitle("Consulta marketplaces")
        
        oDlg:setSize(150,300)

        oDlg:CreateDialog()
        oDlg:addCloseButton(nil, "Fechar")
        oContainer := TPanel():New( ,,, oDlg:getPanelMain() )
        oContainer:Align := CONTROL_ALIGN_ALLCLIENT

        cLine := "{If(aItens[oListBox:nAt,1],oOk,oNo),U_A0601E(aItens[oListBox:nAT][3]),aItens[oListBox:nAT][3]}"
        bLine := &( "{ || " + cLine + " }" )

        oListBox:=TWBrowse():New( 001,001,290,100,,aFields,,oContainer,,,,,,,,,,,,.F.,,.T.,,.F.,,,)
        oListBox:SetArray(aItens)
        oListBox:bLDblClick := { || aItens[oListBox:nAt,1] := !aItens[oListBox:nAt,1] }
        oListBox:lHScroll = .F.
        oListBox:lVScroll = .T.
        oListBox:bLine := bLine

        oDlg:AddButton( 'Confirmar'	,{|| Processa( { || U_A0601G(aItens) } ) ,oDlg:DeActivate() }, 'Confirmar' , , .T., .F., .T., )

        oDlg:addButtons(aButtons)

        oDlg:Activate()

    Else
        Help(,,"Cadastro de marketplaces",,"Não foi encontrado nenhum marketplace ou a API está com problemas.",1,0,,,,,,{"Por favor tente mais tarde."})
    EndIf

Return

User Function A0601B(cCodMkt)
    Local aFields   := { " ", " " ,"Tipo                              ", "Descricao" }
    Local aButtons  := {}
    Local aItens    := {}
    Local oOk       := LoadBitMap(GetResources(), "LBOK")
    Local oNo       := LoadBitMap(GetResources(), "LBNO")

    If !Empty(Alltrim(cCodMkt))
        
        U_A0601H(@aItens,cCodMkt)

        If Len(aItens) > 0
            oDlg := FWDialogModal():New()

            oDlg:SetEscClose(.T.)
            oDlg:SetTitle("Consulta tipos de ocorrências")
            
            oDlg:setSize(150,320)

            oDlg:CreateDialog()
            oDlg:addCloseButton(nil, "Fechar")
            oContainer := TPanel():New( ,,, oDlg:getPanelMain() )
            oContainer:Align := CONTROL_ALIGN_ALLCLIENT

            cLine := "{If(aItens[oListBox:nAt,1],oOk,oNo),U_A0601I(cCodMkt,aItens[oListBox:nAT][3]),aItens[oListBox:nAT][3],aItens[oListBox:nAT][4]}"
            bLine := &( "{ || " + cLine + " }" )

            oListBox:=TWBrowse():New( 001,001,315,100,,aFields,,oContainer,,,,,,,,,,,,.F.,,.T.,,.F.,,,)
            oListBox:SetArray(aItens)
            oListBox:bLDblClick := { || aItens[oListBox:nAt,1] := !aItens[oListBox:nAt,1] }
            oListBox:lHScroll = .F.
            oListBox:lVScroll = .T.
            oListBox:bLine := bLine

            oDlg:AddButton( 'Confirmar'	,{|| Processa( { || U_A0601K(aItens,cCodMkt) } ) ,oDlg:DeActivate() }, 'Confirmar' , , .T., .F., .T., )

            oDlg:addButtons(aButtons)

            oDlg:Activate()

        Else
            Help(,,"Cadastro de tipos",,"Não foi encontrado nenhum tipo.",1,0,,,,,,{"Informe outro marketplace."})
        EndIf
    Else
        Help(,,"Cadastro de tipos",,"O campo Marketplace não está preenchido.",1,0,,,,,,{"Preencha o campo marketplace."})
    EndIf

Return

User Function A0601C(aItens)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX
    Local aMkt      := {}

    
    cUrl := SuperGetMV("MV_YKONURL",.F.,"")
    cPath := "/externalapi/channel/list"
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
            aMkt := Separa(cJson)
        EndIf
    EndIF

    freeObj(oJson)

    For nX := 1 To Len(aMkt)
        cMarket := StrTran(StrTran(StrTran(aMkt[nX],'[',''),']',''),'"','')
        AADD(aItens, { .F., "", cMarket } )
    Next nX

Return

User Function A0601E(cMarket)
    Local oOK       := LoadBitmap(GetResources(),'BR_VERDE')
    Local oNO       := LoadBitmap(GetResources(),'BR_VERMELHO')
    Local oCor
    Local lAchou    := .F.

    lAchou := U_A0601F(cMarket)
    
    If lAchou
        oCor := oOK
    Else
        oCor := oNO
    EndIf

Return(oCor)

User Function A0601F(cMarket)
    Local lRet := .F.
    Local cAlias := GetNextAlias()
    Local cQuery

    cQuery := "SELECT ZZA_MKTID "
    cQuery += "FROM " + RetSqlName("ZZA") + " ZZA "
    cQuery += "WHERE ZZA.D_E_L_E_T_ = ' ' "
    cQuery += "AND ZZA_FILIAL = '" + xFilial("ZZA") + "'
    cQuery += "AND TRIM(ZZA_MKTID) = '" + Alltrim(cMarket) + "'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty(Alltrim((cAlias)->ZZA_MKTID))
        lRet := .T.
    EndIf
    
    (cAlias)->(dbCloseArea())
    
Return(lRet)

User Function A0601G(aItens)
    Local nX
    Local lAchou := .F.
    Local cErro  := ""

    For nX := 1 To Len(aItens)
        If aItens[nX,1]
            lAchou := U_A0601F(aItens[nX,3])
            If lAchou
                cErro += Alltrim(aItens[nX,3]) + "  "
            Else
                Begin Transaction
                dbSelectArea("ZZA")
                ZZA->(RecLock("ZZA",.T.))
                    ZZA->ZZA_FILIAL := xFilial("ZZA")
                    ZZA->ZZA_MKTID  := Alltrim(aItens[nX,3])
                    ZZA->ZZA_BLOQ   := "1"
                ZZA->(dbCloseArea())
                End Transaction
            EndIf
        EndIf
    Next nX

    If !Empty(cErro)
        Help(,,"Cadastro de Marketplace",,"Marketplace " + cErro + " já está cadastrado",1,0,,,,,,{"Os marketplaces não serão incluídos novamente."})
    EndIf

Return

User Function A0601H(aItens,cMarket)
    Local cUrl
    Local cPath
    Local cAuth
    Local aHeader   := {}
    Local oRest
    Local oJson
    Local cParser
    Local nX

    cUrl := SuperGetMV("MV_YKONURL",.F.,"")
    cPath := "/externalapi/releasetype/" + Alltrim(cMarket)
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
                cTipo   := oJson['elements'][nX]['code']
                cDescri := DecodeUTF8(Alltrim(oJson['elements'][nX]['description']))
                AADD( aItens, { .F., "", cTipo, cDescri } )
            Next nX
        EndIf
    EndIF

    freeObj(oJson)

Return

User Function A0601I(cCodMkt,cTipo)
    Local oOK       := LoadBitmap(GetResources(),'BR_VERDE')
    Local oNO       := LoadBitmap(GetResources(),'BR_VERMELHO')
    Local oCor
    Local lAchou    := .F.

    lAchou := U_A0601J(cCodMkt,cTipo)
    
    If lAchou
        oCor := oOK
    Else
        oCor := oNO
    EndIf

Return(oCor)

User Function A0601J(cCodMkt,cTipo)
    Local lRet := .F.
    Local cAlias := GetNextAlias()
    Local cQuery

    cQuery := "SELECT ZZB_MKTID "
    cQuery += "FROM " + RetSqlName("ZZB") + " ZZB "
    cQuery += "WHERE ZZB.D_E_L_E_T_ = ' ' "
    cQuery += "AND ZZB_FILIAL = '" + xFilial("ZZB") + "' "
    cQuery += "AND TRIM(ZZB_MKTID) = '" + Alltrim(cCodMkt) + "' "
    cQuery += "AND TRIM(ZZB_DESCRI) = '" + Alltrim(cTipo) + "'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If !Empty(Alltrim((cAlias)->ZZB_MKTID))
        lRet := .T.
    EndIf
    
    (cAlias)->(dbCloseArea())
    
Return(lRet)

User Function A0601K(aItens,cCodMkt)
    Local nX
    Local oModel    := FwModelActive()
    Local oView     := FwViewActive()
    Local oModelGrd := oModel:GetModel("FILHO1")
    Local nTamGrd   := oModelGrd:Length()
    
    If Len(aItens) > 0
        For nX := 1 To Len(aItens)
            If aItens[nX,1]
                oModelGrd:GoLine(nTamGrd)
                If !Empty(Alltrim(oModelGrd:GetValue('ZZB_MKTID')))
                    oModelGrd:AddLine()
                EndIf
                oModelGrd:LoadValue('ZZB_MKTID'  , cCodMkt)
                oModelGrd:LoadValue('ZZB_DESCRI' , aItens[nX,3])
            EndIf
        Next nX
    EndIf
    
    oModelGrd:SetLine(1)
    oModelGrd:GoLine(1)

    oView:Refresh("FILHO1")

Return
