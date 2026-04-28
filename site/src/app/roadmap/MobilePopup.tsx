'use client';
import './MobilePopup.scss';
import { FC, PropsWithChildren, useEffect, useRef } from 'react';
import UIOverlay from '../../component/UIOverlay/UIOverlay';
import { useIsMobile } from '../../helpers/util';
import { CSSTransition } from 'react-transition-group';

interface MobilePopupProps extends PropsWithChildren {
  show: boolean;
  onClose: () => void;
  className?: string;
  id?: string;
}

const MobilePopup: FC<MobilePopupProps> = ({ show, onClose, className, id, children }) => {
  const isMobile = useIsMobile();
  const overlayRef = useRef<HTMLDivElement>(null);
  const popupRef = useRef<HTMLDivElement>(null);
  const touchStartY = useRef(0);

  useEffect(() => {
    const element = popupRef.current;
    if (!element) return;

    const handleTouchStart = (e: TouchEvent) => {
      touchStartY.current = e.touches[0].clientY;
      element.style.transition = 'none';
    };

    const handleTouchEnd = (e: TouchEvent) => {
      if (e.changedTouches[0].clientY - touchStartY.current > 100) {
        onClose();
      }
      element.style.transform = '';
      element.style.transition = '';
    };

    const handleTouchMove = (e: TouchEvent) => {
      const delta = e.touches[0].clientY - touchStartY.current;
      if (delta > 0) {
        element.style.transform = `translateY(${delta}px)`;
      }
    };

    element.addEventListener('touchstart', handleTouchStart);
    element.addEventListener('touchend', handleTouchEnd);
    element.addEventListener('touchmove', handleTouchMove);

    return () => {
      element.removeEventListener('touchstart', handleTouchStart);
      element.removeEventListener('touchend', handleTouchEnd);
      element.removeEventListener('touchmove', handleTouchMove);
    };
  }, [show, onClose]);

  useEffect(() => {
    if (!isMobile) return;
    overlayRef.current?.classList.toggle('enter-done', show);
    popupRef.current?.classList.toggle('enter-done', show);
  }, [isMobile, show]);

  if (!isMobile) return null;

  return (
    <>
      {isMobile && <UIOverlay onClick={onClose} zIndex={399} ref={overlayRef} />}
      <CSSTransition in={show} timeout={500} unmountOnExit nodeRef={popupRef}>
        <div className={`mobile-popup mobile ${className ?? ''}`} id={id ?? ''} ref={popupRef}>
          {children}
        </div>
      </CSSTransition>
    </>
  );
};

export default MobilePopup;
