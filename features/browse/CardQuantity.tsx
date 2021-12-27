interface CardQuantityProps {
  quantityReg?: number;
  quantityFoil?: number;
}

const CardQuantity: React.FC<CardQuantityProps> = ({ quantityReg = null, quantityFoil = null }) => (
  <>
    {quantityReg !== null && (
      <div>
        x{quantityReg}{' '}
        {quantityFoil !== null && quantityFoil > 0 && (
          <span>
            (+{quantityFoil} {quantityFoil > 1 ? 'foils' : 'foil'})
          </span>
        )}
      </div>
    )}
  </>
);

export default CardQuantity;
