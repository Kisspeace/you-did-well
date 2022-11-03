{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.Threading;

interface
uses
  System.SysUtils, System.Generics.Collections,
  IoUtils, System.Classes, System.Generics.Defaults;

type

  TYdwReusableThread = class(TObject)
    protected
      FThread: TThread;
      FLock: TMREWSync;
      { ❤ -- Thread safe --- }
      function GetIsRunning: boolean;
      procedure ImFinished;
      procedure Start;
      procedure Terminate;
      procedure WaitForFinish;
      { --------------------- }
      procedure Execute; virtual; abstract;
    public
      constructor Create; virtual;
      destructor Destroy; override;
  end;

  TGenericYDWQueuedThread<T> = Class(TYdwReusableThread)
    protected const
      DEFAULT_THREADS_COUNT: integer = 4;
    protected type
      TThreadAndValue<T> = record
        public
          Thread: TThread;
          Value: T;
          constructor Create(AValue: T);
      end;
      { ❤-------------- }
      TThreadAndValueList = TList<TThreadAndValue<T>>;
    protected
    { ❤ -- Thread safe --- }
      procedure SetThreadsCount(const value: integer);
      function GetThreadsCount: integer;
      procedure QueueAdd(AItem: T);
      procedure AbortItem(AItem: T);
      function RunningCount: integer;
    protected
      FThreadsCount: integer;
      FQueue: TList<T>;
      FRunning: TThreadAndValueList;
      function NewSubThread(AValue: T): TThread;
      function QueueCondition: boolean; virtual;
      function AutoRestartCondition: boolean; virtual;
      procedure OnSubThreadFinish; virtual;
      function RunningIndex(AValue: T): integer; virtual; abstract; { dont forget this ! }
      function RunningIndexByThread(const AThread: TThread): integer;
      procedure Execute; override;
      procedure SubThreadExecute(AItem: T); virtual; abstract; { dont forget this ! }
    public
      property ThreadsCount: integer read GetThreadsCount write SetThreadsCount;
      constructor Create; override;
      destructor Destroy; override;
  End;

  TGenericYDWQueuedThreadObject<T: class> = Class(TGenericYDWQueuedThread<T>)
    protected
      function RunningIndex(AValue: T): integer; override;
  End;

  TGenericYDWQueuedThreadInterface<T: IInterface> = Class(TGenericYDWQueuedThread<T>)
    protected
      function RunningIndex(AValue: T): integer; override;
  End;

implementation

{ TYdwQueuedThreadComponent }

function TGenericYDWQueuedThread<T>.AutoRestartCondition: boolean;
begin
  Result := ( Self.QueueCondition ) and
            ( not TThread.Current.CheckTerminated);
end;

procedure TGenericYDWQueuedThread<T>.Execute;
const
  MICRO_SLEEP_TIME = 10;
var
  I: integer;
  LNewThread: TThread;
  LItem: TThreadAndValue<T>;
begin
  try

    try

      while ( TRUE ) do begin

        if TThread.Current.CheckTerminated then exit;

        While ( RunningCount >= Self.ThreadsCount ) do begin
          if TThread.Current.CheckTerminated then exit;
          Sleep(MICRO_SLEEP_TIME);
        end;

        FLock.BeginWrite;
        try
          if ( not Self.QueueCondition ) then
            break;

          LItem := TThreadAndValue<T>.Create(FQueue.First); // get next Value from queue
          LNewThread := NewSubThread(LItem.Value);
          LItem.Thread := LNewThread;
          FRunning.Add(LItem);
          FQueue.Delete(0);
          LNewThread.Start;

          while not LNewThread.Started do
            Sleep(1);

        finally
          FLock.EndWrite;
        end;

      end;

      // waiting for end
      while ( RunningCount > 0 ) do begin
        if TThread.Current.CheckTerminated then exit;
        sleep(MICRO_SLEEP_TIME);
      end;

    finally

      // Terminate all threads
      FLock.BeginWrite;
      try

        for I := 0 to FRunning.count - 1 do begin
          var LThread := FRunning[I].Thread;

          if LThread.Started then
            LThread.Terminate
          else begin
            FRunning[I].Thread.Free;
            var LUpdatedItem := TThreadAndValue<T>.Create(FRunning[I].Value);
            LUpdatedItem.Thread := nil;
            FRunning[I] := LUpdatedItem;
          end;

        end;

        { -- May have issues block begin ! -- }
        var LThPos: integer := FRunning.Count - 1;
        var LTargetPos: integer;

        while TRUE do begin

          LTargetPos := -1;

          if (LThPos < 0) or (FRunning.Count < 1) then
            break;

          for I := LThPos downto 0 do begin
            if not Assigned(FRunning[I].Thread) then begin
              LTargetPos := I;
              break;
            end;
          end;

          if LTargetPos <> -1 then begin
            FRunning.Delete(LTargetPos);
            Dec(LThPos);
          end else
            Break;

          
        end;
        { -- block end. -- }

      finally
        FLock.EndWrite;
      end;

      // Waiting for finish
      while (RunningCount > 0) do begin
        sleep(MICRO_SLEEP_TIME);
      end;

      if Self.AutoRestartCondition then
        Self.Start
      else begin
        Self.ImFinished;
      end;

    end;

  except

    on E: Exception do begin
      Raise E;
    end;

  end;
end;

function TGenericYDWQueuedThread<T>.GetThreadsCount: integer;
begin
  FLock.BeginRead;
  Result := FThreadsCount;
  FLock.EndRead;
end;

function TGenericYDWQueuedThread<T>.NewSubThread(AValue: T): TThread;
begin
  Result := TThread.CreateAnonymousThread(procedure ()
  var
    I, ThreadIndex, LastIndex: integer;
    TempItem: TThreadAndValue<T>;
  begin
    try
      Self.SubThreadExecute(AValue);
    finally
      FLock.BeginWrite;
      try
        LastIndex := FRunning.Count - 1;

        for I := 0 to LastIndex do begin
          if ( FRunning[I].Thread = TThread.Current ) then begin
            ThreadIndex := I;
            Break;
          end;
        end;

        if ( ThreadIndex <> LastIndex ) and ( FRunning.Count > 1 ) then begin
          FRunning.Exchange(LastIndex, ThreadIndex);
        end;

        FRunning.Delete(LastIndex);

      finally
        FLock.EndWrite;
      end;

      Self.OnSubThreadFinish;
    end;
  end);
end;

function TGenericYDWQueuedThread<T>.RunningCount: integer;
begin
  FLock.BeginRead;
  Result := Self.FRunning.Count;
  FLock.EndRead;
end;

procedure TGenericYDWQueuedThread<T>.SetThreadsCount(const Value: integer);
begin
  FLock.BeginWrite;
  try
    if (Value > 0) then
      FThreadsCount := value;
  finally
    FLock.EndWrite;
  end;
end;

{ TYdwReusableThread }

constructor TYdwReusableThread.Create;
begin
  FLock := TMREWSync.Create;
  FThread := Nil;
end;

destructor TYdwReusableThread.Destroy;
begin
  self.Terminate;
  self.WaitForFinish;
  FLock.Free;
  inherited;
end;

function TYdwReusableThread.GetIsRunning: boolean;
begin
  FLock.BeginRead;
  try
    Result := Assigned(FThread);
  finally
    FLock.EndRead;
  end;
end;

procedure TYdwReusableThread.ImFinished;
begin
  FLock.BeginWrite;
  try
    FThread := NIL;
  finally
    FLock.EndWrite;
  end;
end;

procedure TYdwReusableThread.Start;
begin
  FLock.BeginWrite;
  try

    FThread := TThread.CreateAnonymousThread(procedure ()
    begin
      Self.Execute;
    end);

    FThread.Start;

  finally
    FLock.EndWrite;
  end;
end;

procedure TYdwReusableThread.Terminate;
begin
  FLock.BeginWrite;
  try
    if Assigned(FThread) then
      FThread.Terminate;
  finally
    FLock.EndWrite;
  end;
end;

procedure TYdwReusableThread.WaitForFinish;
const
  WAIT_TIMEOUT: integer = 10;
begin
  while GetIsRunning do begin
    if ( TThread.Current.ThreadID = MainThreadId ) then
      CheckSynchronize(WAIT_TIMEOUT)
    else
      Sleep(WAIT_TIMEOUT);
  end;
end;

{ TYdwQueuedThreadComponent.TThreadAndValue<T> }

constructor TGenericYDWQueuedThread<T>.TThreadAndValue<T>.Create(AValue: T);
begin
  Self.Thread := Nil;
  Self.Value := AValue;
end;

{ TGenericYdwQueuedThreadComponent<T> }

procedure TGenericYDWQueuedThread<T>.AbortItem(AItem: T);
var
  Index: integer;
begin
  FLock.BeginWrite;
  try
    Index := Self.RunningIndex(AItem);
    if ( Index <> -1 ) then begin
      try
        FRunning[Index].Thread.Terminate;
      except
        On E: exception do
          raise E;
      end;
    end;

    Index := FQueue.IndexOf(AItem);
    if ( Index <> -1 ) then begin
      FQueue.Delete(Index);
    end;

  finally
    FLock.EndWrite;
  end;
end;

constructor TGenericYDWQueuedThread<T>.Create;
begin
  inherited;
  FQueue := TList<T>.Create;
  FRunning := TThreadAndValueList.Create;
  FThreadsCount := DEFAULT_THREADS_COUNT;
end;

destructor TGenericYDWQueuedThread<T>.Destroy;
begin
  inherited;
  FQueue.Free;
  FRunning.Free;
end;

procedure TGenericYDWQueuedThread<T>.OnSubThreadFinish;
begin
end;

procedure TGenericYDWQueuedThread<T>.QueueAdd(AItem: T);
begin
  FLock.BeginWrite;
  try
    FQueue.Add(AItem);
  finally
    FLock.EndWrite;
  end;

  if not self.GetIsRunning then
    self.Start;
end;

function TGenericYDWQueuedThread<T>.QueueCondition: boolean;
begin
  Result := FQueue.Count > 0;
end;

function TGenericYDWQueuedThread<T>.RunningIndexByThread(
  const AThread: TThread): integer;
var
  I: integer;
begin
   for I := 0 to FRunning.Count - 1 do begin
    if ( FRunning[I].Thread.ThreadID = AThread.ThreadId ) then begin
      Result := I;
      exit;
    end;
  end;
  Result := -1;
end;


{ TObjectYDWQueuedThreadComponent<T> }

function TGenericYDWQueuedThreadObject<T>.RunningIndex(AValue: T): integer;
var
  I: integer;
begin
  for I := 0 to FRunning.Count - 1 do begin
    if ( AValue = FRunning[I].value ) then begin
      Result := I;
      exit;
    end;
  end;
  Result := -1;
end;

{ TInterfaceYDWQueuedThreadComponent<T> }

function TGenericYDWQueuedThreadInterface<T>.RunningIndex(AValue: T): integer;
var
  I: integer;
begin
  for I := 0 to FRunning.Count - 1 do begin
    if ((AValue as TObject) = (FRunning[I].value as TObject )) then begin
      Result := I;
      exit;
    end;
  end;
  Result := -1;
end;

end.
