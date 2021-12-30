import MuiButton, { ButtonProps as MuiButtonProps } from '@material-ui/core/Button';
import CircularProgress from '@material-ui/core/CircularProgress';
import styled from 'styled-components';

const ButtonWrapper = styled.div({
  position: 'relative',
});

const StyledCircularProgress = styled(CircularProgress)(({ theme }) => ({
  color: theme.palette.primary.light,
  position: 'absolute',
  top: '50%',
  left: '50%',
  marginTop: -12,
  marginLeft: -12,
}));

interface ButtonProps extends MuiButtonProps {
  isSubmitting: boolean;
}

const Button: React.FC<ButtonProps> = ({ isSubmitting, children, ...props }) => (
  <ButtonWrapper>
    <MuiButton disabled={isSubmitting} {...props}>
      {children}
    </MuiButton>
    {isSubmitting && <StyledCircularProgress size={24} />}
  </ButtonWrapper>
);

export default Button;
