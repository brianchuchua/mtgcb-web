import { useState } from 'react';
import styled from 'styled-components';
import IconButton from '@material-ui/core/IconButton';

const ToggleableButton = ({ size, children, toggled, handleClick }) => (
  <StyledIconButton size={size} styled={{ toggled }} onClick={() => handleClick()}>
    {children}
  </StyledIconButton>
);

export default ToggleableButton;

const StyledIconButton = styled(IconButton)(({ styled }) => ({
  opacity: styled.toggled ? '1' : '0.3',
}));
