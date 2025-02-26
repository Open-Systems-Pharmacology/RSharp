#' @title NetObject
#' @docType class
#'
#' @description
#' Base wrapper class for the pointers to .NET objects. Offers basic methods to interact with the .NET objects.
#'
#' @importFrom R6 R6Class
NetObject <- R6::R6Class(
  "NetObject",
  cloneable = FALSE,
  active = list(
    #' @field type String representation of the type of the .NET object. Read-only
    type = function(value) {
      if (missing(value)) {
        private$.type
      } else {
        private$.throwPropertyIsReadonly("type")
      }
    },

    #' @field pointer The external pointer to the .NET object. Read-only
    pointer = function(value) {
      if (missing(value)) {
        private$.pointer
      } else {
        private$.throwPropertyIsReadonly("pointer")
      }
    }
  ),
  private = list(
    # The external pointer to the .NET object
    .pointer = NULL,
    # Type of the .NET object as a string
    .type = NULL,

    # @description
    # Simple way to wrap a get;set; .NET property
    # @param name the name of a field/property of the object
    # @param value If a value is passed, `self$set(name, value)` is called. If no value is passed, `self$get(name)` is called.
    # @param shouldSetNull If `value` is `NULL`, should the property be set to `NULL`? Default is `TRUE`. Used for edge-case scenarios where `NULL` is not a valid value for a property.
    # @param asInteger Boolean whether to convert the value to an integer.
    # Used for cases where .NET signature requires an integer. Ignored if `value` is not numeric.
    # @return `self$set(name, value)` if `value` is passed, `self$get(name)` if no `value` is passed.
    .wrapProperty = function(name, value, shouldSetNull = TRUE, asInteger = FALSE) {
      if (missing(value)) {
        return(self$get(name))
      } else {
        # Problem converting reference object to `NULL`
        if (is.null(value) && !shouldSetNull) {
          return()
        }
        return(self$set(name, value, asInteger = asInteger))
      }
    },

    # @description
    # Wrap get/set of a read-only property.
    # Useful in writing class wrappers, but rarely for the end-user
    #
    #
    # @param name the name of a field/property of the object
    # @param value If a value is passed, an error is thrown. If no value is passed, `self$get(name)` is called.
    #
    # @return `self$get(name)` if no `value` is passed, or throws an error if `value` is passed.
    .wrapReadOnlyProperty = function(name, value) {
      if (missing(value)) {
        return(self$get(name))
      } else {
        private$.throwPropertyIsReadonly(name)
      }
    },
    .printLine = function(entry, value = NULL, addTab = TRUE) {
      entries <- paste0(entry, ":", sep = "")

      # helps to visually distinguish class name from its entries
      if (addTab) {
        entries <- c("  ", entries)
      }

      value <- format(value)
      entries <- c(entries, value, "\n")
      cat(entries, sep = " ")
      invisible(self)
    },
    .printClass = function() {
      cat(class(self)[1], ": \n", sep = "")
    },
    .throwPropertyIsReadonly = function(propertyName) {
      stop(messages$errorPropertyReadOnly(propertyName))
    },
    # @description Method called on object destruction
    finalize = function() {
      private$.pointer <- NULL
    }
  ),
  public = list(
    #' @description Initializes the object.
    #' @param pointer The external pointer to the .NET object
    #' @return The initialized object
    #' @export
    #' @examples
    #' testClassName <- "ClrFacade.Tests.RefClasses.LevelOneClass"
    #' o <- .External("r_create_clr_object", testClassName, PACKAGE = getRSharpSetting("nativePkgName"))
    #' x <- newObjectFromName(testClassName)
    #' print(x)
    initialize = function(pointer) {
      .validateIsExtPtr(pointer)
      private$.pointer <- pointer
      # Get the type of the pointer
      private$.type <- .clrTypeNameExtPtr(pointer)
      return(self)
    },

    # is = function(type) {
    #   # call static method once implemented https://github.com/Open-Systems-Pharmacology/rSharp/issues/67
    # },

    #' @description
    #' List the fields of the object
    #'
    #' @param contains a string that the field names returned must contain
    #' @return a list of names of the fields of the object
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getFields()
    #' testObj$getFields("ieldInt")
    getFields = function(contains = "") {
      # Validate contains is string
      callStatic(rSharpEnv$clrFacadeTypeName, "GetInstanceFields", self$pointer, contains)
    },

    #' @description
    #' List the static fields of the object
    #'
    #' @param contains a string that the field names returned must contain
    #' @return a list of names of the static fields of the object
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getStaticFields()
    #' testObj$getStaticFields("ieldInt")
    getStaticFields = function(contains = "") {
      # Validate contains is string
      callStatic(rSharpEnv$clrFacadeTypeName, "GetStaticFields", self$pointer, contains)
    },

    #' @description
    #' List the properties of the object
    #'
    #' @param contains a string that the property names returned must contain
    #' @return a list of names of the properties of the object
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getProperties()
    #' testObj$getProperties("One")
    getProperties = function(contains = "") {
      # Validate contains is string
      callStatic(rSharpEnv$clrFacadeTypeName, "GetInstanceProperties", self$pointer, contains)
    },

    #' @description
    #' List the static properties of the object
    #'
    #' @param contains a string that the property names returned must contain
    #' @return a list of names of the static properties of the object
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getStaticProperties()
    #' testObj$getStaticProperties("One")
    getStaticProperties = function(contains = "") {
      # Validate contains is string
      callStatic(rSharpEnv$clrFacadeTypeName, "GetStaticProperties", self$pointer, contains)
    },

    #' @description
    #' List the methods the object
    #'
    #' @param contains a string that the methods names returned must contain
    #' @return a list of names of the methods of the object
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getMethods()
    #' testObj$getMethods("Get")
    getMethods = function(contains = "") {
      # Validate contains is string
      callStatic(rSharpEnv$clrFacadeTypeName, "GetInstanceMethods", self$pointer, contains)
    },

    #' @description
    #' List the static methods the object
    #'
    #' @param contains a string that the methods names returned must contain
    #' @return a list of names of the static methods of the object
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getStaticMethods()
    #' testObj$getStaticMethods("Get")
    getStaticMethods = function(contains = "") {
      # Validate contains is string
      callStatic(rSharpEnv$clrFacadeTypeName, "GetStaticMethods", self$pointer, contains)
    },

    #' @description
    #' Gets a string representation of the signature of a member (i.e. field, property, method).
    #' Mostly used to interactively search for what arguments to pass to a method.
    #'
    #' @param memberName The exact name of the member (i.e. field, property, method) to search for
    #' @return a character vector with summary information on the method/member signatures
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$getMemberSignature("set_PropertyIntegerOne")
    #' testObj$getMemberSignature("FieldIntegerOne")
    #' testObj$getMemberSignature("PropertyIntegerTwo")
    getMemberSignature = function(memberName) {
      callStatic(rSharpEnv$clrFacadeTypeName, "GetSignature", self$pointer, memberName)
    },

    #' @description
    #' Call a method of the object
    #'
    #' @param methodName the name of a method of the object
    #' @param ... additional method arguments
    #' @return An object resulting from the call. May be a `NetObject` object, or a native R object for common types. Can be NULL.
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$call("GetFieldIntegerOne")
    call = function(methodName, ...) {
      # validateIsString methodName
      # First I implemented checking for the method name in the list of methods,
      # but the the test for explicitly implemented methods fails. The check must
      # be performed in C++ https://github.com/Open-Systems-Pharmacology/rSharp/issues/70
      # if (!any(methodName == self$getMethods())) {
      #   stop(messages$errorMethodNotFound(methodName, self$type))
      # }

      # Extract the pointer for R6 objects
      args <- .extractPointersFromArgs(list(...))
      # Calling via `do.call` to pass the arguments
      result <- do.call(".External", c(list("r_call_method", self$pointer, methodName), args, PACKAGE = rSharpEnv$nativePkgName))
      return(castToRObject(result))
    },

    #' @description
    #' Gets the value of a field or property of the object
    #'
    #' @param name the name of a field/property  of the object
    #' @return An object resulting from the call. May be a `NetObject` object, or a native R object for common types. Can be NULL.
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$get("FieldIntegerOne")
    get = function(name) {
      # Internally calling `getStatic` because it uses the same C++ code
      return(getStatic(self$pointer, name))
    },

    #' @description
    #' Sets the value of a field or property of the object.
    #'
    #' @param name the name of a field/property of the object
    #' @param value the value to set the field with
    #' @param asInteger Boolean whether to convert the value to an integer.
    #' Used for cases where .NET signature requires an integer. Ignored if `value` is not numeric.
    #' @export
    #' @examples
    #' testClassName <- getRSharpSetting("testObjectTypeName")
    #' testObj <- newObjectFromName(testClassName)
    #' testObj$set("FieldIntegerOne", as.integer(42))
    set = function(name, value, asInteger = FALSE) {
      # Internally calling `setStatic` because it uses the same C++ code
      invisible(setStatic(self$pointer, name, value, asInteger))
    },

    #' @description Prints a summary of the object.
    print = function() {
      private$.printClass()
      private$.printLine("Type", private$.type)
      private$.printLine("Methods:", self$getMethods())
      private$.printLine("Fields", self$getFields())
      private$.printLine("Properties", self$getProperties())
      invisible(self)
    }
  )
)
