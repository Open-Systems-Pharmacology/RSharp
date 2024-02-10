﻿using System;
using System.Linq;
using System.Runtime.InteropServices;
using RDotNet;
using RDotNet.Internals;

namespace ClrFacade
{
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    public delegate IntPtr ClrObjectToSexpDelegate(IntPtr variant);

    public interface IUnmanagedDll
    {
        ClrObjectToSexpDelegate ClrObjectToSexp { get; set; }
        IntPtr GetFunctionAddress(string entryPointName);
    }

    /// <summary>
    /// A helper class to inspect data and determind what it is converted to in the unmanaged code.
    /// </summary>
    /// <remarks>
    /// Acknowledgements go to Lim Bio Liong for some of this code. See http://limbioliong.wordpress.com/2011/09/04/using-variants-in-managed-code-part-1/ 
    /// and  http://limbioliong.wordpress.com/2011/03/20/c-interop-how-to-return-a-variant-from-an-unmanaged-function/. 
    /// Very useful and impressive series of articles.
    /// </remarks>
    public static class DataConversionHelper
    {
        // c:\Program Files\Windows Kits\8.0\Include\um\OAIdl.h
        //typedef /* [wire_marshal] */ struct tagVARIANT VARIANT;
        //struct tagVARIANT
        [StructLayout(LayoutKind.Sequential)]
        public struct Variant
        {
            public ushort vt;
            public ushort wReserved1;
            public ushort wReserved2;
            public ushort wReserved3;
            public Int32 data01;
            public Int32 data02;
        }
       
        public static IUnmanagedDll RclrNativeDll= null;

        [Obsolete("Could not make to work overall (it works itself, just not in the method is it associated with)", true)]
        static IntPtr ClrObjectToSexp(IntPtr variant)
        {
            if (variant == IntPtr.Zero)
                return IntPtr.Zero;
            if (RclrNativeDll == null)
                return ClrObjectToSexpMs(variant);
            return RclrNativeDll.ClrObjectToSexp(variant);
        }

        const Int32 SizeOfNativeVariant = 16;

        [DllImport(@"rClrMs.dll", EntryPoint = "clr_object_to_SEXP", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr ClrObjectToSexpMs(IntPtr variant);

        /// <summary>
        /// Creates a pointer to a native SEXP. This method is for advanced operations, 
        /// where garbage collections are impacted. Gurus only.
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public static IntPtr ClrObjectToSexp(object obj)
        {
           throw new NotImplementedException();
           //try
           //{
           //    // Trying to solve https://rclr.codeplex.com/workitem/33. 
           //    // Not sure whether I can ignore the returned handle or not, however, so treat as experimental.
           //    //GCHandle.Alloc(obj, GCHandleType.Pinned);
           //    // pVariant = CreateNativeVariantForObject(obj);
           //    // return ClrObjectToSexp(pVariant);
           //}
           //catch
           //{
           //    // We want to deallocate memory on error, but not on successful completion.
           //    // since the creation of a native variant for obj is what creates a handle in the CLR hosting
           //    // to prevent the garbage collection.
           //    //FreeVariantMem(pVariant);
           //    throw;
           //}
        }

        private static string GetVariantTypeString( IntPtr pVariant)
        {
            VarEnum vt = GetVariantType(pVariant);
            return GetVariantTypeString(vt);
        }

        private static VarEnum GetVariantType(IntPtr pVariant)
        {
            Variant v = GetManagedVariant(pVariant);
            VarEnum vt = (VarEnum)(v.vt);
            return vt;
        }

        private static Variant GetManagedVariant(IntPtr pVariant)
        {
            Variant v = (Variant)Marshal.PtrToStructure(pVariant, typeof(Variant));
            return v;
        }

        private const bool useCoTaskMem = true;


        private static string GetVariantTypeString(VarEnum vt)
        {
            VarEnum e;
            var vtNames = Enum.GetNames(typeof(VarEnum));
            foreach (var vtn in vtNames)
            {
                e = (VarEnum)Enum.Parse(typeof(VarEnum), vtn);
                if (e == vt)
                    return vtn;
                else if (vt == (VarEnum.VT_ARRAY | e))
                    return string.Concat("VT_ARRAY | ", vtn);
            }
            throw new NotSupportedException(string.Format("Could not find a valid VARIANT type for VarEnum code {0}", (int)vt));
        }
    }

    /// <summary>
    /// A wrapper around a symbolic expression. This is necessary to wrap safehandle around.
    /// </summary>
    public class SymbolicExpressionWrapper
    {
        private static int counter = 0;
        public SymbolicExpressionWrapper(SymbolicExpression sexp)
        {
            this.Sexp = sexp;
            counter++;
        }
        public SymbolicExpression Sexp { get; private set; }

        ~SymbolicExpressionWrapper()
        {
            counter--;
        }

        public object ToClrEquivalent()
        {
            switch (Sexp.Type)
            {
                case SymbolicExpressionType.CharacterVector:
                    return convertVector(Sexp.AsCharacter().ToArray());
                case SymbolicExpressionType.ComplexVector:
                    return convertVector(Sexp.AsComplex().ToArray());
                case SymbolicExpressionType.IntegerVector:
                    return convertVector(Sexp.AsInteger().ToArray());
                case SymbolicExpressionType.LogicalVector:
                    return convertVector(Sexp.AsLogical().ToArray());
                case SymbolicExpressionType.NumericVector:
                    return convertNumericVector(Sexp);
                case SymbolicExpressionType.RawVector:
                    return convertVector(Sexp.AsRaw().ToArray());
                // case SymbolicExpressionType.S4:
                //     {
                //         var s4sxp = Sexp.AsS4();
                //         if (!s4sxp.HasSlot("clrobj")) return Sexp;
                //         var fromIntPtr = GCHandle.FromIntPtr(s4sxp["clrobj"].DangerousGetHandle());
                //         return fromIntPtr.Target;
                //     }
                case SymbolicExpressionType.List:
                    return convertVector(convertList(Sexp.AsList().ToArray()));
                default:
                    return Sexp;
            }
        }

      private static object convertNumericVector(SymbolicExpression sexp)
      {
         var values = sexp.AsNumeric().ToArray();
         var classNames = RDotNetDataConverter.GetClassAttrib(sexp);
         if (classNames != null)
         {
            if (classNames.Contains("Date"))
               return convertVector(RDateToDateTime(values));
            if (classNames.Contains("POSIXct"))
               return convertVector(RPosixctToDateTime(sexp, values));
            if (classNames.Contains("difftime"))
               return convertVector(RdifftimeToTimespan(sexp, values));
         }
         return convertVector(values);

      }

      private static DateTime RDateOrigin = new DateTime(1970, 1, 1);

      private static string[] _timediffUnits = new[]{/*"auto",*/ "secs", "mins", "hours",
                   "days", "weeks"};

      private static TimeSpan[] RdifftimeToTimespan(SymbolicExpression sexp, double[] values)
      {
         var units = RDotNetDataConverter.GetAttrib(sexp, "units")[0];
         if (!_timediffUnits.Contains(units)) throw new NotSupportedException("timediff units {0} are not supported");
         if (units == "secs") return Array.ConvertAll(values, TimeSpan.FromSeconds);
         if (units == "mins") return Array.ConvertAll(values, TimeSpan.FromMinutes);
         if (units == "hours") return Array.ConvertAll(values, TimeSpan.FromHours);
         if (units == "days") return Array.ConvertAll(values, TimeSpan.FromDays);
         if (units == "weeks") return Array.ConvertAll(values, x => TimeSpan.FromDays(x * 7));
         // This should never be reached.
         throw new NotSupportedException();
      }

      private static DateTime[] RPosixctToDateTime(SymbolicExpression sexp, double[] values)
      {
         var tz = RDotNetDataConverter.GetTzoneAttrib(sexp);
         if (!isSupportedTimeZone(tz))
            throw new NotSupportedException("POSIXct conversion supported only for UTC or unspecified (local) time zone, not for " + tz);

         //number of seconds since 1970-01-01 UTC
         return Array.ConvertAll(values,
                 v =>
                 {
                    bool utc = isUtc(tz);
                    return global::ClrFacade.ClrFacade.ForceDateKind(RDateOrigin + TimeSpan.FromTicks((long)(TimeSpan.TicksPerSecond * v)), utc);
                 }
             );
      }

      private static bool isSupportedTimeZone(string tz)
      {
         return isUtc(tz) || string.IsNullOrEmpty(tz);
      }

      private static bool isUtc(string tz)
      {
         if (string.IsNullOrEmpty(tz))
            return false;
         var t = tz.ToUpper();
         return (t == "UTC" || t == "GMT");
      }

      private static DateTime[] RDateToDateTime(double[] values)
      {
         //number of days since 1970-01-01
         return Array.ConvertAll(values, v => RDateOrigin + TimeSpan.FromTicks((long)(TimeSpan.TicksPerDay * v)));
      }

      private object[] convertList(SymbolicExpression[] symbolicExpression)
        {
            // Fall back on Renable vecsxp in C layer;
            throw new NotSupportedException("Not supported; would need to be able to unpack e.g. S4 objects.");
        }



        private static object convertVector<T>(T[] p)
        {
            if (p.Length == 1)
                return p[0];
            else
                return p;
        }
    }
}
