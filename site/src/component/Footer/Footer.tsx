import React, { FC } from 'react'
import './Footer.scss'

const Footer: FC = (props) => {
    return (
        <>
            <footer className='footer'>
                <div className='links'>
                    <a href='https://github.com/icssc-projects'>Github</a>
                    <a href='https://api.peterportal.org'>API</a>
                </div>
                <div className='copyright'>
                    <p>Made with ♥ by <a href='https://studentcouncil.ics.uci.edu/'>ICSSC Project Committee</a></p>
                </div>
            </footer>
        </>
    );
}

export default Footer;
