{ * ------------------------------------------------------------------------
  * ♥
  * ♥  Delphi DataSetProxy component - wrapper for Delphi TDataSet
  * ♥
  * Home: https://github.com/bogdanpolak/delphi-dataproxy
  *
  * Classes:
  *   1. TGenericDataSetProxy - base class for the wrapper
  *   2. TDatasetProxy - inherited from TGenericDataSetProxy adding ForEach
  *   3. TDataProxyFactory - TDasetProxy and derived clasess factory
  *  ----------------------------------------------------------------------- * }
unit Data.DataProxy;

interface

uses
  System.Classes,
  System.SysUtils,
  Data.DB;

type
  TGenericDataSetProxy = class(TComponent)
  const
    // * --------------------------------------------------------------------
    // * Component Signature
    ReleaseDate = '2019.09.28';
    ReleaseVersion = '1.0';
    // * --------------------------------------------------------------------
  private
  protected
    FDataSet: TDataSet;
    procedure ConnectFields; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // -------------
    procedure ConnectWithDataSet(aDataSet: TDataSet);
    // -------------
    procedure Append;
    procedure AppendRecord(const Values: array of const);
    procedure BindToDataSource(DataSource: TDataSource);
    procedure Cancel;
    function ConstructDataSource(AOwner: TComponent): TDataSource;
    procedure Close;
    function ControlsDisabled: Boolean;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    procedure Delete;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    procedure First;
    procedure Insert;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean;
    procedure Last;
    function Eof: Boolean;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    procedure Next;
    procedure Open;
    procedure Post;
    procedure Prior;
    procedure Refresh;
    function UpdateStatus: TUpdateStatus;
  end;

  TDatasetProxy = class(TGenericDataSetProxy)
    procedure ForEach(OnElem: TProc);
  end;

  TDataProxyFactory = class
    class function CreateProxy<T: TDatasetProxy>(Owner: TComponent;
      aDataSet: TDataSet): T;
  end;

implementation

// * --------------------------------------------------------------------
// * TGenericDataSetProxy
// * --------------------------------------------------------------------

procedure TGenericDataSetProxy.Append;
begin
  FDataSet.Append;
end;

procedure TGenericDataSetProxy.AppendRecord(const Values: array of const);
begin
  FDataSet.AppendRecord(Values);
end;

procedure TGenericDataSetProxy.BindToDataSource(DataSource: TDataSource);
begin
  DataSource.DataSet := FDataSet;
end;

procedure TGenericDataSetProxy.Cancel;
begin
  FDataSet.Cancel;
end;

procedure TGenericDataSetProxy.Close;
begin
  FDataSet.Close;
end;

procedure TGenericDataSetProxy.ConnectWithDataSet(aDataSet: TDataSet);
begin
  FDataSet := aDataSet;
end;

function TGenericDataSetProxy.ConstructDataSource(AOwner: TComponent)
  : TDataSource;
begin
  Result := TDataSource.Create(AOwner);
  Result.DataSet := FDataSet;
end;

function TGenericDataSetProxy.ControlsDisabled: Boolean;
begin
  Result := FDataSet.ControlsDisabled;
end;

constructor TGenericDataSetProxy.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: Implement
end;

function TGenericDataSetProxy.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := FDataSet.CreateBlobStream(Field, Mode);
end;

procedure TGenericDataSetProxy.Delete;
begin
  FDataSet.Delete;
end;

destructor TGenericDataSetProxy.Destroy;
begin

  inherited;
end;

procedure TGenericDataSetProxy.DisableControls;
begin
  FDataSet.DisableControls;
end;

procedure TGenericDataSetProxy.Edit;
begin
  FDataSet.Edit;
end;

procedure TGenericDataSetProxy.EnableControls;
begin
  FDataSet.EnableControls;
end;

function TGenericDataSetProxy.Eof: Boolean;
begin
  Result := FDataSet.Eof;
end;

procedure TGenericDataSetProxy.First;
begin
  FDataSet.First;
end;

procedure TGenericDataSetProxy.Insert;
begin
  FDataSet.Insert;
end;

procedure TGenericDataSetProxy.InsertRecord(const Values: array of const);
begin
  FDataSet.InsertRecord(Values);
end;

function TGenericDataSetProxy.IsEmpty: Boolean;
begin
  Result := FDataSet.IsEmpty;
end;

procedure TGenericDataSetProxy.Last;
begin
  FDataSet.Last;
end;

function TGenericDataSetProxy.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := FDataSet.Locate(KeyFields, KeyValues, Options);
end;

function TGenericDataSetProxy.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  Result := FDataSet.Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TGenericDataSetProxy.Next;
begin
  FDataSet.Next;
end;

procedure TGenericDataSetProxy.Open;
begin
  FDataSet.Open;
  ConnectFields;
end;

procedure TGenericDataSetProxy.Post;
begin
  FDataSet.Post;
end;

procedure TGenericDataSetProxy.Prior;
begin
  FDataSet.Prior;
end;

procedure TGenericDataSetProxy.Refresh;
begin
  FDataSet.Refresh;
end;

function TGenericDataSetProxy.UpdateStatus: TUpdateStatus;
begin
  Result := FDataSet.UpdateStatus;
end;


// * --------------------------------------------------------------------
// * TDatasetProxy
// * --------------------------------------------------------------------

procedure TDatasetProxy.ForEach(OnElem: TProc);
var
  Bookmark: TBookmark;
begin
  self.DisableControls;
  try
    Bookmark := FDataSet.GetBookmark;
    try
      self.First;
      while not FDataSet.Eof do
      begin
        OnElem();
        FDataSet.Next;
      end;
    finally
      if FDataSet.BookmarkValid(Bookmark) then
        FDataSet.GotoBookmark(Bookmark);
      FDataSet.FreeBookmark(Bookmark);
    end;
  finally
    FDataSet.EnableControls;
  end;
end;


// * --------------------------------------------------------------------
// * TDataProxyFactory
// * --------------------------------------------------------------------

class function TDataProxyFactory.CreateProxy<T>(Owner: TComponent;
  aDataSet: TDataSet): T;
begin
  Result := T.Create(Owner);
  Result.ConnectWithDataSet(aDataSet);
  Result.Open;
end;

end.
