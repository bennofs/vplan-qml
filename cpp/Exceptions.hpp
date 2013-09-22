////////////////////////////////////////////////////////////////////////////////////////////////////
/// \file
/// \brief Defines exceptions types used in vplan-qml.
///
/// This file defines all the exception types that are used in the project. New exception types
/// should always derive from std::exception.
////////////////////////////////////////////////////////////////////////////////////////////////////

#pragma once

#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////////////////////////
/// \brief This type of exception is thrown whenever some operation on a file goes wrong, for
///        example, if loading a file fails to parse.
////////////////////////////////////////////////////////////////////////////////////////////////////
struct FileError : public std::runtime_error {

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief Construct a FileError.
    ///
    /// \param[in] operation The operation that caused the error, in progressive form. 
    ///            Example: "Loading", "Saving", ...
    /// \param[in] file The file that was processed.
    /// \param[in] message Description of what went wrong. 
    ///            Example: "File not found", "Permission denied", ...
    ////////////////////////////////////////////////////////////////////////////////////////////////
    FileError(std::string operation, std::string file, std::string message) : std::runtime_error(operation + " file " + file + " failed: " + message), 
                                                                              file(file), operation(operation) {}

    std::string file;
    std::string operation;
};
