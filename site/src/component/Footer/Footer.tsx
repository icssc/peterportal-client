import { FC } from 'react';
import './Footer.scss';

const Footer: FC = () => {
  return (
    <>
      <footer className="footer">
        <div className="links">
          <a href="https://github.com/icssc" target="_blank" rel="noreferrer">
            GitHub
          </a>
          <a href="https://docs.api-next.peterportal.org" target="_blank" rel="noreferrer">
            API
          </a>
          <a href="https://forms.gle/JjwBmELq26daroTh9" target="_blank" rel="noreferrer">
            Feedback
          </a>
        </div>
        <div className="copyright">
          <p>
            ♥{' '}
            <a href="https://icssc.club/" target="_blank" rel="noreferrer">
              ICSSC
            </a>
          </p>
        </div>
      </footer>
    </>
  );
};

export default Footer;
