type event_id

type create_flags =
  [ `None
  | `UseCFTypes
  | `NoDefer
  | `WatchRoot
  | `IgnoreSelf
  | `FileEvents ]

type event_flags =
  [ `None
  | `MustScanSubDirs
  | `UserDropped
  | `KernelDropped
  | `EventIdsWrapped
  | `HistoryDone
  | `RootChanged
  | `Mount
  | `Unmount
  | `ItemCreated
  | `ItemRemoved
  | `ItemInodeMetaMod
  | `ItemRenamed
  | `ItemModified
  | `ItemFinderInfoMod
  | `ItemChangeOwner
  | `ItemXattrMod
  | `ItemIsFile
  | `ItemIsDir
  | `ItemIsSymlink ]

type callback = (string * event_flags * event_id) list -> unit

type stream

val create: latency:float -> flags:create_flags -> string list -> callback -> stream
