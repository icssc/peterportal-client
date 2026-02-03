import { FC, useState, useEffect } from 'react';
import './ScrollToTopButton.scss';
// import Button from '@mui/material/Button';
import IconButton from '@mui/material/IconButton';
import KeyboardArrowUpIcon from '@mui/icons-material/KeyboardArrowUp';

interface ScrollToTopButtonProps {
  scrollableTarget: string;
  size?: 'medium' | 'large';
}

const ScrollToTopButton: FC<ScrollToTopButtonProps> = ({ scrollableTarget, size = 'medium' }) => {
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    const el = document.getElementById(scrollableTarget);
    if (!el) return;

    const onScroll = () => {
      setVisible(el.scrollTop > 300);
    };

    el.addEventListener('scroll', onScroll);
    return () => el.removeEventListener('scroll', onScroll);
  }, [scrollableTarget]);

  const jumpToTop = () => {
    document.getElementById(scrollableTarget)?.scrollTo({ top: 0, behavior: 'smooth' });
  };

  if (!visible) return null;

  return (
    <IconButton onClick={jumpToTop} className="scroll-to-top-button" size={size}>
      <KeyboardArrowUpIcon />
    </IconButton>
  );

  // return <Button onClick={jumpToTop} className="scroll-to-top-button" startIcon={<KeyboardArrowUpIcon />} />;
};

export default ScrollToTopButton;
