# SSH
1. Generar una llave ssh: `ssh-keygen`
2. Ver la llave publica: `cat ~/.ssh/id_rsa.pub`

# Checklist para empezar un Laboratorio
1. Asegurarse de estar en master: `git checkout master`
    - Si git alega de cambios pendientes, hacer
    `git commit -m "algun mensaje"` antes hacer el
    `git checkout master`
2. Jalar los cambios del repositorio principal: `git pull clase master`
3. Actualizar repositorio propio: `git push`
4. Crear una rama para trabajar en el lab: `git checkout -b labX`