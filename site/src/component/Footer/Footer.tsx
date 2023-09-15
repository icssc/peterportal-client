import React, { FC } from 'react'
import './Footer.scss'

const Footer: FC = () => {
    return (
        <>
            <footer className='footer'>
                <div className='links'>
                    <a href='https://github.com/icssc'>GitHub</a>
                    <a href='https://docs.api-next.peterportal.org'>API</a>
                </div>
                <div className='copyright'>
                    <p>Made with ♥ by <a href='https://icssc.club/'>ICSSC Projects Committee</a></p>
                </div>
            </footer>
        </>
    );
}

export default Footer;
