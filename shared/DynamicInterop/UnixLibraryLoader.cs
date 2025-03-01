﻿using System;
using System.Runtime.ConstrainedExecution;
using System.Runtime.InteropServices;
using System.Security.Permissions;

namespace DynamicInterop
{
   [SecurityPermission(SecurityAction.Demand, Flags = SecurityPermissionFlag.UnmanagedCode)]
   internal class UnixLibraryLoader : IDynamicLibraryLoader
   {
      public IntPtr LoadLibrary(string filename)
      {
         const int RTLD_LAZY = 0x1;

         if (_so == 0)
            return InternalLoadLibrary(filename, RTLD_LAZY);

         if (_so == 1)
            return dlopen1(filename, RTLD_LAZY);

         return dlopen2(filename, RTLD_LAZY);
      }

      /// <summary>
      ///    Gets the last error. NOTE: according to http://tldp.org/HOWTO/Program-Library-HOWTO/dl-libraries.html, returns NULL
      ///    if called more than once after dlopen.
      /// </summary>
      /// <returns>The last error.</returns>
      public string GetLastError()
      {
         if (_so == 0)
            throw new Exception(checkLoadLibraryStatus("GetLastError"));

         if (_so == 1)
            return dlerror1();

         return dlerror2();
      }

      private static string checkLoadLibraryStatus(string callingFunctionName)
      {
         return $"Called {callingFunctionName} before LoadLibrary";
      }

      /// <summary>
      ///    Unloads a library
      /// </summary>
      /// <param name="handle">The pointer resulting from loading the library</param>
      /// <returns>True if the function dlclose returned 0</returns>
      public bool FreeLibrary(IntPtr handle)
      {
         if (_so == 0)
            throw new Exception(checkLoadLibraryStatus("FreeLibrary"));

         // according to the manual page on a Debian box
         // The function dlclose() returns 0 on success, and nonzero on error.
         if (_so == 1)
         {
            var status = dlclose1(handle);
            return status == 0;
         }
         else
         {
            var status = dlclose2(handle);
            return status == 0;
         }
      }

      public IntPtr GetFunctionAddress(IntPtr hModule, string lpProcName)
      {
         if (_so == 0)
            throw new Exception(checkLoadLibraryStatus("GetFunctionAddress"));

         if (_so == 1)
            return dlsym1(hModule, lpProcName);

         return dlsym2(hModule, lpProcName);
      }

      internal static IntPtr InternalLoadLibrary(string filename, int lazy)
      {
         try
         {
            var result = dlopen1(filename, lazy);
            _so = 1;
            return result;
         }
         catch (DllNotFoundException)
         {
            _so = 2;
         }

         return dlopen2(filename, lazy);
      }

      private static int _so;

      [DllImport("libdl", EntryPoint = "dlopen")]
      private static extern IntPtr dlopen1([MarshalAs(UnmanagedType.LPStr)] string filename, int flag);

      [DllImport("libdl.so.2", EntryPoint = "dlopen")]
      private static extern IntPtr dlopen2([MarshalAs(UnmanagedType.LPStr)] string filename, int flag);

      [DllImport("libdl", EntryPoint = "dlerror")]
      [return: MarshalAs(UnmanagedType.LPStr)]
      private static extern string dlerror1();

      [DllImport("libdl.so.2", EntryPoint = "dlerror")]
      [return: MarshalAs(UnmanagedType.LPStr)]
      private static extern string dlerror2();

      [DllImport("libdl", EntryPoint = "dlclose")]
      [ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)]
      private static extern int dlclose1(IntPtr hModule);

      [DllImport("libdl.so.2", EntryPoint = "dlclose")]
      [ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)]
      private static extern int dlclose2(IntPtr hModule);

      [DllImport("libdl", EntryPoint = "dlsym")]
      private static extern IntPtr dlsym1(IntPtr hModule, [MarshalAs(UnmanagedType.LPStr)] string lpProcName);

      [DllImport("libdl.so.2", EntryPoint = "dlsym")]
      private static extern IntPtr dlsym2(IntPtr hModule, [MarshalAs(UnmanagedType.LPStr)] string lpProcName);
   }
}