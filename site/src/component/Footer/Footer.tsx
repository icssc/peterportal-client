import { FC } from 'react';
import './Footer.scss';

const Footer: FC = () => {
  return (
    <>
      <footer className="footer">
        <div className="links">
          <a href="https://github.com/icssc">GitHub</a>
          <a href="https://docs.api-next.peterportal.org">API</a>
          <a href="https://forms.gle/JjwBmELq26daroTh9">Feedback</a>
        </div>
        <div className="copyright">
          <p>
            ♥ <a href="https://icssc.club/">ICSSC</a>
          </p>
        </div>
      </footer>
    </>
  );
};

export default Footer;
