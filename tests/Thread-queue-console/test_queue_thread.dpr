program test_queue_thread;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, YDW.Threading;

type

  TEntity = Class(TObject)
    public
      Count: integer;
      Msg: string;
      procedure Say;
  End;

  TForum = Class(TGenericYDWQueuedThreadObject<TEntity>)
    protected
      procedure SubThreadExecute(AItem: TEntity); override;
    public
      procedure AddSpeaker(AMsg: string; ACount: integer);
  End;

var
  WriteSync: TMREWSync;
procedure SyncWrite(AText: string);
begin
  WriteSync.BeginWrite;
  Writeln(AText);
  WriteSync.EndWrite;
end;

{ TEntity }

procedure TEntity.Say;
var
  I: integer;
begin
  for I := 1 to Count do begin
    SyncWrite(Self.GetHashCode.ToString + '[' + I.ToString + '] : ' + Msg);
    sleep(Random(220));
  end;
end;

{ TForum }

procedure TForum.AddSpeaker(AMsg: string; ACount: integer);
var
  LNewSpeaker: TEntity;
begin
  LNewSpeaker := TEntity.Create;
  LNewSpeaker.Count := ACount;
  LNewSpeaker.Msg := AMsg;
  Self.QueueAdd(LNewSpeaker);
end;

procedure TForum.SubThreadExecute(AItem: TEntity);
begin
  try
    try
      AItem.Say;
    except
      On E: Exception do
        Writeln('SubThreadExecute - ' + E.ClassName, ': ', E.Message);
    end;
  finally
    AItem.Free;
  end;
end;

begin
  try
    WriteSync := TMREWSync.Create;
    var LForum: TForum := TForum.Create;
    LForum.ThreadsCount := 10;

    with LForum do begin
      AddSpeaker('Hey!', 9);
      AddSpeaker('Hola', 19);
      AddSpeaker('How are u', 3);
      AddSpeaker('what ?', 10);
      AddSpeaker('hello there', 3);
      AddSpeaker('AAA !!!', 7);
    end;

    LForum.WaitFor;
    LForum.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Writeln('Fin.');
  Readln;
end.
