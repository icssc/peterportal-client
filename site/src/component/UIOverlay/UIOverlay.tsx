const UIOverlay = ({
  zIndex,
  ...props
}: { zIndex: number; passedRef?: React.RefObject<HTMLDivElement> } & JSX.IntrinsicElements['div']) => {
  const passedRef = props.passedRef;
  delete props.passedRef;
  // Clicking this is only an alternative action to something that is already accessible
  return <div className="ui-overlay" {...props} ref={passedRef} style={{ zIndex }}></div>;
};

export default UIOverlay;
