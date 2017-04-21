object WebModule2: TWebModule2
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule2DefaultHandlerAction
    end
    item
      Name = 'WebActionItem1'
      PathInfo = '/table'
      OnAction = WebModule2WebActionItem1Action
    end>
  Height = 230
  Width = 415
  object RlxRazorProcessor1: TRlxRazorProcessor
    InputFilename = './table.html'
    UserLoggedIn = False
    LanguageId = 0
    Left = 120
    Top = 120
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 48
    Top = 40
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 176
    Top = 40
  end
end
