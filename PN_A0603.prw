#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'

//Variáveis Estáticas
Static cTitulo := "Títulos resolvidos na Koncili"
Static cAlias1 := "ZZC"

/*/{Protheus.doc} A0603
    Função que mostra os títulos resolvidos na Koncili
    @type  Function
    @author Pono Tecnologia
    @since 29/09/2022
    @version 12.1.33+
    /*/
User Function A0603()
    Local oBrowse
    Private lRpc    := Type("cFilAnt") == "U"
    Private aRotina := MenuDef()

    If lRpc
		RPCSetType(3)
		RpcSetEnv('01','02',,,,GetEnvServer(),{ })
	Else 
        If cFilAnt == "01"
            Aviso("Atenção!","Operação não permitida para a MATRIZ.",{"Ok"})
            Return
        EndIf
    EndIf

    oBrowse := FWMBrowse():New()
    oBrowse:SetAlias(cAlias1)
    oBrowse:SetDescription(cTitulo)
    oBrowse:Activate()
    
Return

/*/{Protheus.doc} MenuDef
    Função para montagem do menu
    @type Function
    @author Pono Tecnologia
    @since 30/09/2022
    @version 12.1.33
    /*/
Static Function MenuDef()
    Local aRotina := {}

    ADD OPTION aRotina TITLE "Pesquisar"  	            ACTION 'PesqBrw' 		  OPERATION 1 ACCESS 0
    ADD OPTION aRotina TITLE "Visualizar" 	            ACTION "VIEWDEF.PN_A0603" OPERATION 2 ACCESS 0
    ADD OPTION aRotina TITLE "Incluir"    	            ACTION "VIEWDEF.PN_A0603" OPERATION 3 ACCESS 0
    ADD OPTION aRotina TITLE "Alterar"    	            ACTION "VIEWDEF.PN_A0603" OPERATION 4 ACCESS 0
    ADD OPTION aRotina TITLE "Excluir"    	            ACTION "VIEWDEF.PN_A0603" OPERATION 5 ACCESS 0
    ADD OPTION aRotina TITLE "Imprimir" 	            ACTION "VIEWDEF.PN_A0603" OPERATION 8 ACCESS 0

Return aRotina

/*/{Protheus.doc} ModelDef
    Função para montar o modelo
    @type Function
    @author Pono Tecnologia
    @since 30/09/2022
    @version 12.1.33
    /*/
Static Function ModelDef()
    Local oModel := Nil
    Local oMaster  := FWFormStruct( 1, cAlias1 )

    oModel := MPFormModel():New('PN_A0603M',,,)
    oModel:addFields("MASTER", Nil, oMaster)
    oModel:SetPrimaryKey({"ZZC_FILIAL", "ZZC_MARKET", "ZZC_IDCONC", "ZZC_TITULO", "ZZC_CLIENT", "ZZC_LOJA" })
    oModel:SetDescription(cTitulo)
    oModel:GetModel( "MASTER" ):SetDescription( "Formulário de cadastro" + cTitulo ) 
	
Return oModel

/*/{Protheus.doc} ViewDef
    Função para montar a visuzalização
    @type Function
    @author Cristiano Roberto Giacomozzi
    @since 30/09/2022
    @version 12.1.23
    /*/
Static Function ViewDef()
    Local oModel:= FWLoadModel( "PN_A0603" )
    Local oMaster	:= FWFormStruct( 2, cAlias1 )
    Local oView := Nil
    
    oView := FWFormView():New()
    oView:SetModel(oModel)
    oView:AddField( "VIEW_MASTER" , oMaster , 'MASTER' )
    oView:EnableTitleView('VIEW_MASTER', 'Títulos resolvidos na Koncili' )
    oView:SetCloseOnOk({||.T.})
    oView:EnableControlBar(.T.)
	
Return oView
