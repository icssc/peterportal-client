import React from "react";

const ThemeContext = React.createContext<{ darkMode: boolean, toggleTheme: () => void }>({
    darkMode: false,
    toggleTheme: () => {}
});

export default ThemeContext;