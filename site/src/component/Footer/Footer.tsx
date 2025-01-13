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
          <a href="https://docs.icssc.club/docs/developer/anteaterapi" target="_blank" rel="noreferrer">
            API
          </a>
          <a
            href="https://form.asana.com/?k=4h9ZTRkVUT9ZwfJrmvxDDw&d=1208267282546207"
            target="_blank"
            rel="noreferrer"
          >
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
