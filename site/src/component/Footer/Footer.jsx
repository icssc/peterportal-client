import React from 'react'
import './Footer.scss'

function Footer(props) {
    return (
        <>
            <footer className='footer'>
                <div>
                    <a href='https://github.com/icssc-projects'>Github</a>
                    <a href='/api/v1'>API</a>
                    <a href='/about'>About</a>
                    <a href='/about#team'>Team</a>
                    <a href='/legal'>Terms</a>
                    <a href='/legal'>Privacy Policy</a>
                </div>
                <div className='copyright'>
                    <p>Made with ♥ by <a href='https://studentcouncil.ics.uci.edu/'>ICSSC Project Committee</a></p>
                </div>
            </footer>
        </>
    );
}

export default Footer;