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
  const popupTouchStartY = useRef(0);
  const overlayTouchStartY = useRef(0);

  useEffect(() => {
    const element = popupRef.current;
    const overlay = overlayRef.current;
    let isScrolling: boolean = false;

    if (!element) return;

    const handleTouchStart = (e: TouchEvent) => {
      if (element.scrollTop !== 0) {
        isScrolling = true;
      }
      popupTouchStartY.current = e.touches[0].clientY;
      element.style.transition = 'none';
    };

    const handleTouchEnd = (e: TouchEvent) => {
      if (e.changedTouches[0].clientY - popupTouchStartY.current > 400 && !isScrolling) {
        onClose();
      }
      element.style.transform = '';
      element.style.transition = '';
      isScrolling = false;
    };

    const handleTouchMovePopup = (e: TouchEvent) => {
      const delta = e.touches[0].clientY - popupTouchStartY.current;
      if (!isScrolling) {
        e.preventDefault();
        element.style.transform = delta > 0 ? `translateY(${delta}px)` : 'translateY(0)';
      }
    };

    const handleOverLayTouchStart = (e: TouchEvent) => {
      overlayTouchStartY.current = e.touches[0].clientY;
      element.style.transition = 'none';
    };

    const handleOverlayTouchEnd = (e: TouchEvent) => {
      if (e.changedTouches[0].clientY - overlayTouchStartY.current > 50) {
        onClose();
      }
      element.style.transform = '';
      element.style.transition = '';
    };

    const handleOverlayTouchMove = (e: TouchEvent) => {
      const delta = e.touches[0].clientY - overlayTouchStartY.current;
      if (delta > 0) {
        element.style.transform = `translateY(${delta}px)`;
      }
    };

    element?.addEventListener('touchstart', handleTouchStart);
    element?.addEventListener('touchend', handleTouchEnd);
    element?.addEventListener('touchmove', handleTouchMovePopup, { passive: false });

    overlay?.addEventListener('touchstart', handleOverLayTouchStart);
    overlay?.addEventListener('touchend', handleOverlayTouchEnd);
    overlay?.addEventListener('touchmove', handleOverlayTouchMove);

    return () => {
      element.removeEventListener('touchstart', handleTouchStart);
      element.removeEventListener('touchend', handleTouchEnd);
      element.removeEventListener('touchmove', handleTouchMovePopup, { passive: false } as EventListenerOptions);
      overlay?.removeEventListener('touchstart', handleOverLayTouchStart);
      overlay?.removeEventListener('touchend', handleOverlayTouchEnd);
      overlay?.removeEventListener('touchmove', handleOverlayTouchMove);
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
