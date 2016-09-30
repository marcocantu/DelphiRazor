object WebModule13: TWebModule13
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <
    item
      Name = 'WebActionItem1'
      PathInfo = '/test1'
      Producer = RlxRazorProcessor1
    end
    item
      Name = 'WebActionItem2'
      PathInfo = '/test2'
      Producer = RlxRazorProcessor2
    end
    item
      Name = 'WebActionItem3'
      PathInfo = '/test3'
      OnAction = WebModule13WebActionItem3Action
    end
    item
      Name = 'waValueFromEvent'
      PathInfo = '/valuefromevent'
      Producer = RlxRazorValueFromEvent
    end
    item
      Name = 'waValueFromObj'
      PathInfo = '/valuefromobject'
      OnAction = WebModule13WebActionItem5Action
    end
    item
      Name = 'waCompany'
      PathInfo = '/company'
      OnAction = WebModule13waCompanyAction
    end
    item
      Name = 'waIf'
      PathInfo = '/if'
      OnAction = WebModule13waIfAction
    end
    item
      Name = 'waCompanyList'
      PathInfo = '/list'
      OnAction = WebModule13waCompanyListAction
    end
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      Producer = RlxRazorProcessor1
      OnAction = WebModule13DefaultHandlerAction
    end>
  Height = 449
  Width = 602
  object RlxRazorProcessor1: TRlxRazorProcessor
    InputFilename = '..\..\html\test.html'
    OnLang = RlxRazorProcessor1Lang
    UserLoggedIn = False
    LanguageId = 0
    Left = 112
    Top = 48
  end
  object RlxRazorEngine1: TRlxRazorEngine
    FilesFolder = '..\..\html\'
    TemplatesFolder = '..\..\html\'
    HomePage = 'test.html'
    OnLang = RlxRazorEngine1Lang
    Left = 288
    Top = 48
  end
  object RlxRazorProcessor2: TRlxRazorProcessor
    InputFilename = '..\..\html\test.html'
    RazorEngine = RlxRazorEngine1
    UserLoggedIn = False
    LanguageId = 0
    Left = 288
    Top = 112
  end
  object RlxRazorProcessor3: TRlxRazorProcessor
    InputFilename = '..\..\html\test3.html'
    RazorEngine = RlxRazorEngine1
    UserLoggedIn = False
    LanguageId = 0
    Left = 288
    Top = 176
  end
  object RlxRazorValueFromEvent: TRlxRazorProcessor
    InputFilename = '..\..\html\getvalue.html'
    OnValue = RlxRazorValueFromEventValue
    UserLoggedIn = False
    LanguageId = 0
    Left = 96
    Top = 136
  end
  object RlxRazorValueFromObject: TRlxRazorProcessor
    InputFilename = '..\..\html\getvalue.html'
    OnValue = RlxRazorValueFromEventValue
    UserLoggedIn = False
    LanguageId = 0
    Left = 104
    Top = 216
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FileName = '..\..\html\customer.cds'
    Params = <>
    Left = 320
    Top = 256
  end
  object RlxRazorValueFromTable: TRlxRazorProcessor
    InputFilename = '..\..\html\company.html'
    OnValue = RlxRazorValueFromEventValue
    UserLoggedIn = False
    LanguageId = 0
    Left = 104
    Top = 288
  end
  object RlxRazorIf: TRlxRazorProcessor
    InputFilename = '..\..\html\if.html'
    OnValue = RlxRazorValueFromEventValue
    UserLoggedIn = False
    LanguageId = 0
    Left = 112
    Top = 360
  end
  object RlxRazorCompanyList: TRlxRazorProcessor
    InputFilename = '..\..\html\companylist.html'
    OnValue = RlxRazorValueFromEventValue
    UserLoggedIn = False
    LanguageId = 0
    Left = 248
    Top = 336
  end
end
