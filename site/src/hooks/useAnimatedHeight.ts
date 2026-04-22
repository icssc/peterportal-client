import { type RefObject, useEffect, useRef } from 'react';

export function useAnimatedHeight(ref: RefObject<HTMLElement | null>) {
  const previousHeight = useRef<number | null>(null);
  const isAnimating = useRef(false);

  useEffect(() => {
    const el = ref.current;
    if (!el) return;

    const observer = new ResizeObserver((entries) => {
      if (isAnimating.current) return;

      for (const entry of entries) {
        const newHeight = entry.borderBoxSize[0].blockSize;
        const oldHeight = previousHeight.current;

        if (oldHeight !== null && Math.abs(oldHeight - newHeight) > 1) {
          isAnimating.current = true;
          el.style.height = `${oldHeight}px`;
          requestAnimationFrame(() => {
            el.style.height = `${newHeight}px`;
            const onEnd = () => {
              el.style.height = '';
              isAnimating.current = false;
              previousHeight.current = newHeight;
              el.removeEventListener('transitionend', onEnd);
            };
            el.addEventListener('transitionend', onEnd, { once: true });
          });
        } else {
          previousHeight.current = newHeight;
        }
      }
    });

    observer.observe(el);
    return () => observer.disconnect();
  }, [ref]);
}
