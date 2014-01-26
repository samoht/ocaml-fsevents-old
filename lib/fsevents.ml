open Ctypes
open PosixTypes
open Foreign

let bool =
  view ~read:((<>)0) ~write:(fun b -> compare b false) int;;

module CFIndex = struct

  (* typedef signed long CFIndex; *)
  let _t = long

  let of_int = Signed.Long.of_int

  let to_int = Signed.Long.to_int

  let t = view ~read:to_int ~write:of_int _t

end

module CFString = struct

  (* typedef const struct __CFString *CFStringRef; *)
  type __CFString = unit ptr
  let __CFString: __CFString typ = ptr void
  let _t = ptr __CFString

  (* typedef UInt32 CFStringEncoding; *)
  let encoding = uint32_t

  (* enum CFStringBuiltInEncodings {
       kCFStringEncodingMacRoman = 0,
       kCFStringEncodingWindowsLatin1 = 0x0500,
       kCFStringEncodingISOLatin1 = 0x0201,
       kCFStringEncodingNextStepLatin = 0x0B01,
       kCFStringEncodingASCII = 0x0600,
       kCFStringEncodingUnicode = 0x0100,
       kCFStringEncodingUTF8 = 0x08000100,
       kCFStringEncodingNonLossyASCII = 0x0BFF,

       kCFStringEncodingUTF16 = 0x0100,
       kCFStringEncodingUTF16BE = 0x10000100,
       kCFStringEncodingUTF16LE = 0x14000100,
       kCFStringEncodingUTF32 = 0x0c000100,
       kCFStringEncodingUTF32BE = 0x18000100,
       kCFStringEncodingUTF32LE = 0x1c000100
    };
    typedef enum CFStringBuiltInEncodings CFStringBuiltInEncodings;
  *)
  let ascii = Unsigned.UInt32.of_int32 0x0600l

  (* CFIndex CFStringGetLength (
       CFStringRef theString
    );
  *)
  let get_length =
    foreign "CFStringGetLength" (_t @-> returning CFIndex.t)

  (* CFStringRef CFSTR (
        const char *cStr
    );
  *)
  let cfstr =
    foreign "CFSTR" (string @-> returning _t)

  let of_string = cfstr

  (* Boolean CFStringGetCString (
      CFStringRef theString,
      char *buffer,
      CFIndex bufferSize,
      CFStringEncoding encoding
    ); *)
  let get_C_string =
    foreign "CFStringGetCString" (
      _t @->
      ptr char @->
      CFIndex.t @->
      encoding @->
      returning bool
    )

  let to_chars t =
    let n = get_length t in
    let a = Array.make char n in
    let _ = get_C_string t (Array.start a) n ascii in
    a

  let string_of_chars a =
    (* XXX: how can we do better ? *)
    let n = Array.length a in
    let s = String.create n in
    for i = 0 to n - 1 do s.[i] <- (Array.get a i) done;
    s

  let to_string t =
    string_of_chars (to_chars t)

  let t =
    view ~read:to_string ~write:of_string _t

  let string_of_ptr p =
    let t = !@ (from_voidp _t p) in
    to_string t

end

module CFRange = struct

  (* struct CFRange {
       CFIndex location;
       CFIndex length;
     };
     typedef struct CFRange CFRange;
  *)
  type range
  let _t: range structure typ = structure "CFRange"
  let location = field _t "location" CFIndex.t
  let length = field _t "length" CFIndex.t
  let () = seal _t

  type t = {
    location: int;
    length: int;
  }

  let of_t { location = loc; length = len } =
    let t = make _t in
    setf t location loc;
    setf t length len;
    t

  let to_t t =
    let location = getf t location in
    let length = getf t length in
    { location; length }

  let t = view ~read:to_t ~write:of_t _t

end

module CFArray = struct

  (* typedef const struct __CFArray *CFArrayRef; *)
  type __CFArray = unit ptr
  let __CFArray: __CFArray typ = ptr void
  let _t = ptr __CFArray

  (* CFIndex CFArrayGetCount (
      CFArrayRef theArray
    );
  *)
  let get_count =
    foreign "CFArrayGetCount" (_t @-> returning CFIndex.t)

  (* void CFArrayGetValues (
       CFArrayRef theArray,
       CFRange range,
       const void **values
     );
  *)
  let get_values =
    foreign "CFArrayGetValues" (
      _t @->
      CFRange.t @->
      returning (ptr (ptr void))
    )

  let to_array t =
    let n = get_count t in
    let r = { CFRange.location = 0; length = n } in
    Array.from_ptr (get_values t r) n

  (* CFArrayRef CFArrayCreate (
       CFAllocatorRef allocator,
       const void **values,
       CFIndex numValues,
       const CFArrayCallBacks *callBacks
     );
  *)
  let create =
    foreign "CFArrayCreate" (
      ptr_opt void @->
      ptr (ptr void) @->
      CFIndex.t @->
      ptr_opt void @->
      returning _t)

  let of_array a =
    let n = Array.length a in
    create None (Array.start a) n None

  let t = view ~read:to_array ~write:of_array _t

end

module CFTimeInterval = struct

  (* typedef double CFTimeInterval; *)
  let t = double

end

module CFAllocate = struct

  (* typedef void ( *CFAllocatorReleaseCallBack) (
     const void *info
     );
  *)
  let release_callback = funptr (ptr void @-> returning void)

  (* typedef const void *( *CFAllocatorRetainCallBack) (
     const void *info
     );
  *)
  let retain_callback = funptr (ptr void @-> returning (ptr void))

  (* typedef CFStringRef ( *CFAllocatorCopyDescriptionCallBack) (
     const void *info
     );
  *)
  let copy_description_callback = funptr (ptr void @-> returning CFString.t)

end

(* typedef const struct __FSEventStream* ConstFSEventStreamRef; *)
type __FSEventStream = unit ptr
let __FSEventStream: __FSEventStream typ = ptr void
let const_ref = ptr __FSEventStream

module Create_flags = struct

  type t =
    [ `None
    | `UseCFTypes
    | `NoDefer
    | `WatchRoot
    | `IgnoreSelf
    | `FileEvents ]

  (* enum {
     kFSEventStreamCreateFlagNone = 0x00000000,
     kFSEventStreamCreateFlagUseCFTypes = 0x00000001,
     kFSEventStreamCreateFlagNoDefer = 0x00000002,
     kFSEventStreamCreateFlagWatchRoot = 0x00000004,
     kFSEventStreamCreateFlagIgnoreSelf = 0x00000008,
     kFSEventStreamCreateFlagFileEvents = 0x00000010
   };
   typedef UInt32 FSEventStreamCreateFlags; *)
  let of_int i =
    match Unsigned.UInt32.to_int32 i with
    | 0x00000000l -> `None
    | 0x00000001l -> `UseCFTypes
    | 0x00000002l -> `NoDefer
    | 0x00000004l -> `WatchRoot
    | 0x00000008l -> `IgnoreSelf
    | 0x00000010l -> `FileEvents
    | _           -> assert false

  let to_int t =
    Unsigned.UInt32.of_int32 (
      match t with
      | `None       -> 0x00000000l
      | `UseCFTypes -> 0x00000001l
      | `NoDefer    -> 0x00000002l
      | `WatchRoot  -> 0x00000004l
      | `IgnoreSelf -> 0x00000008l
      | `FileEvents -> 0x00000010l
    )

  let t = view ~read:of_int ~write:to_int uint32_t

end

type create_flags = Create_flags.t

module Event_flags = struct

  (* typedef UInt32 FSEventStreamEventFlags;

     enum {
     kFSEventStreamEventFlagNone = 0x00000000,
     kFSEventStreamEventFlagMustScanSubDirs = 0x00000001,
     kFSEventStreamEventFlagUserDropped = 0x00000002,
     kFSEventStreamEventFlagKernelDropped = 0x00000004,
     kFSEventStreamEventFlagEventIdsWrapped = 0x00000008,
     kFSEventStreamEventFlagHistoryDone = 0x00000010,
     kFSEventStreamEventFlagRootChanged = 0x00000020,
     kFSEventStreamEventFlagMount = 0x00000040,
     kFSEventStreamEventFlagUnmount = 0x00000080, /* These flags are only
         set if you specified the FileEvents*/ /* flags when creating the
         stream.*/ kFSEventStreamEventFlagItemCreated = 0x00000100,
     kFSEventStreamEventFlagItemRemoved = 0x00000200,
     kFSEventStreamEventFlagItemInodeMetaMod = 0x00000400,
     kFSEventStreamEventFlagItemRenamed = 0x00000800,
     kFSEventStreamEventFlagItemModified = 0x00001000,
     kFSEventStreamEventFlagItemFinderInfoMod = 0x00002000,
     kFSEventStreamEventFlagItemChangeOwner = 0x00004000,
     kFSEventStreamEventFlagItemXattrMod = 0x00008000,
     kFSEventStreamEventFlagItemIsFile = 0x00010000,
     kFSEventStreamEventFlagItemIsDir = 0x00020000,
     kFSEventStreamEventFlagItemIsSymlink = 0x00040000 };
  *)

  type t =
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

  let to_int t =
    Unsigned.UInt32.of_int32 (
      match t with
      | `None              -> 0x00000000l
      | `MustScanSubDirs   -> 0x00000001l
      | `UserDropped       -> 0x00000002l
      | `KernelDropped     -> 0x00000004l
      | `EventIdsWrapped   -> 0x00000008l
      | `HistoryDone       -> 0x00000010l
      | `RootChanged       -> 0x00000020l
      | `Mount             -> 0x00000040l
      | `Unmount           -> 0x00000080l
      | `ItemCreated       -> 0x00000100l
      | `ItemRemoved       -> 0x00000200l
      | `ItemInodeMetaMod  -> 0x00000400l
      | `ItemRenamed       -> 0x00000800l
      | `ItemModified      -> 0x00001000l
      | `ItemFinderInfoMod -> 0x00002000l
      | `ItemChangeOwner   -> 0x00004000l
      | `ItemXattrMod      -> 0x00008000l
      | `ItemIsFile        -> 0x00010000l
      | `ItemIsDir         -> 0x00020000l
      | `ItemIsSymlink     -> 0x00040000l
    )

  let of_int i =
    match Unsigned.UInt32.to_int32 i with
    | 0x00000000l -> `None
    | 0x00000001l -> `MustScanSubDirs
    | 0x00000002l -> `UserDropped
    | 0x00000004l -> `KernelDropped
    | 0x00000008l -> `EventIdsWrapped
    | 0x00000010l -> `HistoryDone
    | 0x00000020l -> `RootChanged
    | 0x00000040l -> `Mount
    | 0x00000080l -> `Unmount
    | 0x00000100l -> `ItemCreated
    | 0x00000200l -> `ItemRemoved
    | 0x00000400l -> `ItemInodeMetaMod
    | 0x00000800l -> `ItemRenamed
    | 0x00001000l -> `ItemModified
    | 0x00002000l -> `ItemFinderInfoMod
    | 0x00004000l -> `ItemChangeOwner
    | 0x00008000l -> `ItemXattrMod
    | 0x00010000l -> `ItemIsFile
    | 0x00020000l -> `ItemIsDir
    | 0x00040000l -> `ItemIsSymlink
    | _           -> assert false

  let t = view ~read:of_int ~write:to_int uint32_t

end

type event_flags = Event_flags.t

(* typedef UInt64 FSEventStreamEventId; *)
let event_id = uint64_t
let since_now = !@ (foreign_value "kFSEventStreamEventIdS" event_id)
type event_id = int64

(* typedef struct __FSEventStream* FSEventStreamRef; *)
let stream = ptr __FSEventStream
type stream = __FSEventStream ptr

(*
typedef void ( *FSEventStreamCallback )(
   ConstFSEventStreamRef streamRef,
   void *clientCallBackInfo,
   size_t numEvents,
   void *eventPaths,
   const FSEventStreamEventFlags eventFlags[],
   const FSEventStreamEventId eventIds[]);
*)
let callback =
  funptr (
    const_ref @->
    ptr void @->
    size_t @->
    ptr (ptr void) @->
    ptr Event_flags.t @->
    ptr event_id @->
    returning void
  )
type callback = (string * event_flags * event_id) list -> unit

let create_callback (create_flags:create_flags) (fn:callback) =
  fun stream client_callback_info num_events event_paths event_flags event_ids ->
    let n = Unsigned.Size_t.to_int num_events in
    let paths = Array.(from_ptr event_paths n) in
    let string (ptr:unit ptr) = match create_flags with
      | `UseCFTypes -> CFString.string_of_ptr ptr
      | _           -> failwith "TODO" in
    let flags = Array.(from_ptr event_flags n) in
    let ids = Array.(from_ptr event_ids n) in
    let l = ref [] in
    for i = 0 to n - 1 do
      l := ( string (Array.get paths i),
             Array.get flags i,
             Unsigned.UInt64.to_int64 (Array.get ids i)
           ) :: !l
    done;
    fn (List.rev !l)

(* struct FSEventStreamContext {
     CFIndex version;
     void *info;
     CFAllocatorRetainCallBack retain;
     CFAllocatorReleaseCallBack release;
     CFAllocatorCopyDescriptionCallBack copyDescription;
  }; *)
type context
let context: context structure typ = structure "FSEventStreamContext"
let context_version = field context "version" CFIndex.t
let context_info = field context "info" (ptr void)
let context_retain = field context "retain" CFAllocate.retain_callback
let context_release = field context "release" CFAllocate.release_callback
let context_copy_description = field context "copyDescription" CFAllocate.copy_description_callback
let () = seal context

(*
extern FSEventStreamRef FSEventStreamCreate(
   CFAllocatorRef allocator,
   FSEventStreamCallback callback,
   FSEventStreamContext *context,
   CFArrayRef pathsToWatch,
   FSEventStreamEventId sinceWhen,
   CFTimeInterval latency,
   FSEventStreamCreateFlags flags);
*)
let create =
  foreign "FSEventStreamCreate"
    (ptr_opt void @->
     callback @->
     ptr_opt context @->
     CFArray.t @->
     event_id @->
     CFTimeInterval.t @->
     Create_flags.t @->
     returning stream)

let create ~latency ~(flags:create_flags) (paths:string list) (callback:callback): stream =
  let paths = List.map (fun str ->
      to_voidp (CFString.of_string str)
    ) paths in
  let paths = Array.of_list (ptr void) paths in
  create None (create_callback flags callback) None paths since_now latency flags
