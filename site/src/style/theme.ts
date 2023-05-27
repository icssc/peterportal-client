export const lightTheme: Theme = {
    name: 'light',
    textColor: '#212529',
    textColorDark: '#000',
    inputGroupText: '#495057',
    backgroundColor: 'var(--peterportal-gray-blue)',
    overlay1Color: 'var(--ring-road-white)',
    overlay2Color: 'var(--peterportal-gray-blue)',
    quarterTitleColor: '#202e47',
    navbarBoxShadow: '0px 4px 24px rgba(196, 198, 209, 0.24)',
    prereqNodeTextColor: 'rgba(0,0,0,.6)',
    prereqNodeBackgroundColor: '#e0e1e2'
}

export const darkTheme: Theme = {
    name: 'dark',
    textColor: '#fff',
    textColorDark: '#fff',
    inputGroupText: '#fff',
    backgroundColor: '#121212',
    overlay1Color: '#1E1E1E',
    overlay2Color: '#292929',
    quarterTitleColor: '#eee',
    navbarBoxShadow: '0px 4px 24px rgba(0, 0, 0, 0.24)',
    prereqNodeTextColor: '#fff',
    prereqNodeBackgroundColor: '#292929'
}

interface Theme {
    name: string,
    textColor: string,
    textColorDark: string,
    inputGroupText: string,
    backgroundColor: string,
    overlay1Color: string,
    overlay2Color: string,
    quarterTitleColor: string,
    navbarBoxShadow: string,
    prereqNodeTextColor: string,
    prereqNodeBackgroundColor: string
}

export default Theme;