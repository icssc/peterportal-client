const UIOverlay = ({
  zIndex,
  ...props
}: { zIndex: number; passedRef?: React.RefObject<HTMLDivElement> } & JSX.IntrinsicElements['div']) => {
  // Clicking this is only an alternative action to something that is already accessible
  return <div className="ui-overlay" {...props} ref={props.passedRef} style={{ zIndex }}></div>;
};

export default UIOverlay;
